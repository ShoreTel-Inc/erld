/*
	erld - A UNIX-style daemon wrapper for Erlang
	Copyright (C) 2012 ShoreTel Inc.

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#define _XOPEN_SOURCE 500 /* strdup, vsnprintf etc. */
#define _DARWIN_C_SOURCE /* getting WCORESUMP macro on mac */
#include <string.h>
#include <signal.h> /* kill, signal */
#include <sys/types.h>
#include <sys/wait.h> /* waitpid */
#include <syslog.h> /* syslog */
#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc, free */
#include "util.h"
#include "debug.h"

char *alloc_vprintf(const char *fmt, va_list ap) {
	int l;
	char *msg;
	va_list tmp_ap;

	if (!fmt)
		return 0;
	va_copy(tmp_ap, ap);
	l = vsnprintf(0, 0, fmt, tmp_ap);
	va_end(tmp_ap);
	if (l < 0)
		return strdup("alloc_vprintf error");
	msg = (char*)malloc(l + 1);
	va_copy(tmp_ap, ap);
	vsnprintf(msg, l + 1, fmt, tmp_ap);
	va_end(tmp_ap);
	return msg;
}

char *alloc_printf(const char *fmt, ...) {
	va_list ap;
	char *msg;

	va_start(ap, fmt);
	msg = alloc_vprintf(fmt, ap);
	va_end(ap);
	return msg;
}

void set_cleanup_handler(void (*handler)(int)) {
	set_signal(SIGQUIT, handler, 0);
	set_signal(SIGINT, handler, 0);
	set_signal(SIGTSTP, handler, 0);
	set_signal(SIGTERM, handler, 0);
}

/* Use sigaction() here because we need a received signal
 * to interrupt waitpid() or select(), and that isn't always
 * the case with signal(). */
void set_signal(int signum, void (*handler)(int), int flags) {
	struct sigaction sa;

	memset(&sa, 0, sizeof sa);
	sa.sa_handler = handler;
	sa.sa_flags = flags;
	CHECK(sigaction(signum, &sa, 0));
}

int cleanup_child(int sig, const char *child_name, pid_t child_pid) {
	int rv, status, exit_code = 0;
	char *exit_status;

	if (child_pid == -1)
		return 0;
	/* If we're here because of a signal (arg != 0), pass it on to erld */
	if (sig) {
		LOG_V("passing signal %d to %s", sig, child_name);
		kill(child_pid, sig);
	}
	DEBUG_V("Waiting for %s (pid %d) to exit.", child_name, child_pid);
	while (((rv = waitpid(child_pid, &status, 0)) == -1) && (errno == EINTR))
		/* empty */;
	CHECK(rv);
	get_exit_status(status, &exit_status, &exit_code);
	DEBUG("Wait complete.");
	LOG_V("%s exited with status %d: %s.", child_name, exit_code, exit_status);
	syslog(LOG_INFO, "%s exited with status %d: %s.", child_name, exit_code, exit_status);
	free(exit_status);
	return exit_code;
}

void get_exit_status(int status, char **exit_status, int *exit_code) {
	if (WIFEXITED(status)) {
		*exit_status =  alloc_printf("exited with code %d", WEXITSTATUS(status));
		if (exit_code)
			*exit_code = WEXITSTATUS(status);
	}
	else if (WIFSIGNALED(status)) {
		if(WCOREDUMP(status))
			*exit_status = alloc_printf("terminated by signal %d (core dumped)", WTERMSIG(status));
		else
			*exit_status = alloc_printf("terminated by signal %d (no core dump)", WTERMSIG(status));
	}
	else
		*exit_status = alloc_printf("exited for unknown reason (no exit code or singal)");
}
