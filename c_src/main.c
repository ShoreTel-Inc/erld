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

// #define _XOPEN_SOURCE 500 /* Causes waitpid to be interrupted by signals (SIGUSR1). */ TODO

// Mac has no O_LARGEFILE parameter - I believe by default all files are 'large files'.
#ifdef __APPLE__
#define O_LARGEFILE 0
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <getopt.h>

#include "version.h"
#include "debug.h"
#include "global.h"
#include "log.h"
#include "pid.h"
#include "util.h"
#include "erld.h"
#include "cnode.h"

/* values that must be global because they are accessed in cleanup (signal) handlers */
static pid_t erld_pid = -1;
static int daemonised = 0;

/* options */
int foreground = 0;
struct timeval heartbeat_timeout;
struct timeval heartbeat_warn;
struct timeval grace_period;
int restart_limit = 0;
struct timeval restart_interval;

void usage();
void version();
int terminal_main(int erld_pid);
void cleanup_terminal(int arg);
void daemonise(int arg);

int main(int argc, char *const argv[]) {
	char c, *cookie = 0;

	timerclear(&heartbeat_timeout);
	timerclear(&heartbeat_warn);
	timerclear(&grace_period);
	grace_period.tv_sec = DEFAULT_GRACE_PERIOD;
	timerclear(&restart_interval);
	while ((c = getopt(argc, argv, "fl:p:c:dn:t:T:r:i:g:M:F:hv")) != -1) {
		switch (c) {
			case 'f': foreground = 1; break;
			case 'l': log_file_name = strdup(optarg); break;
			case 'p': pid_file_name = strdup(optarg); break;
			case 'c': cookie = strdup(optarg); break;
			case 'd': debug_flag = 1; break;
			case 'n': node_name = strdup(optarg); break;
			case 't': heartbeat_timeout.tv_sec = atol(optarg); break;
			case 'T': heartbeat_warn.tv_sec = atol(optarg); break;
			case 'r': restart_limit = atoi(optarg); break;
			case 'i': restart_interval.tv_sec = atol(optarg); break;
			case 'g': grace_period.tv_sec = atol(optarg); break;
			case 'M': log_rotation_module = strdup(optarg); break;
			case 'F': log_rotation_function = strdup(optarg); break;
			case 'h': usage(); exit(0); break;
			case 'v': version(); exit(0); break;
			default: printf("Unknown option: %c\n", c);
		}
	}

	if (optind >= argc) {
		usage();
		exit(0);
	}

	if (debug_flag)
		ei_tracelevel = 99;

	set_signal(SIGUSR1, daemonise, 0);

	if (foreground) {
		DEBUG("going directly to erld_main");
		return erld_main(cookie, argv + optind);
	}
	else {
		DEBUG_V("forking to start %s", NAME);
		if (!(erld_pid = fork())) {
			set_signal(SIGUSR1, SIG_DFL, 0); // Clear the SIGUSR1 handler we've inherited.
			return erld_main(cookie, argv + optind);
		}
		set_cleanup_handler(cleanup_terminal);
		return terminal_main(erld_pid);
	}
}

/* This is the main loop of the original process (main).
 * It just sits at the terminal, waiting to be told to
 * exit with a particular exit code (either by an explicit
 * signal or the death of it's child. */
int terminal_main(int erld_pid) {
	int rv, status;
	char *status_message;

	DEBUG("waiting for daemonize");
	while (1) {
		DEBUG("waitpid");
		rv = waitpid(erld_pid, &status, 0);
		DEBUG("wait complete");
		if (daemonised) {
			/* If we've got the OK, we don't care what happened to end the wait */
			DEBUG(NAME " startup successful, exiting.");
			exit(0);
		}
		if (rv == -1) {
			if (errno != EINTR)
				FATAL_ERROR(1, "waitpid");
		}
		else {
			if (rv == erld_pid) {
				DEBUG(NAME " has exited");
				if (!WIFEXITED(status) || (WEXITSTATUS(status) != 1)) {
					/* A status of 1 indicates that the daemon has already written an error message */
					/* otherwise... */
					get_exit_status(status, &status_message, 0);
					fprintf(stderr, "Startup failed, " NAME " crashed: %s\n", status_message);
					free(status_message);
				}
				exit(1);
			}
			else
				DEBUG_V("unknown child (pid %d) has exited, ignored", rv);
		}
	}
}

void usage() {
	printf("%s [options] -- <erlang command line>\n", NAME);
	printf(
	"-f run in the foreground\n"
	"-l <log file>, default: \"%s\"\n"
	"-p <pid file name>, default: not used\n"
	"-c <cookie file>, default: $HOME/%s\n"
	"-n <node name>, default: %s-<PID>\n"
	"-t <heartbeat timeout in seconds>, default: disabled\n"
	"-T <heartbeat warning in seconds>, default: disabled\n"
	"-r <restart limit>, default: unlimited\n"
	"-i <time defining a short restart>, default: unlimited\n"
	"-g <kill grace period in seconds>, default: %d\n"
	"-M <log rotation module name>, default: %s\n"
	"-M <log rotation function name>, default: %s\n"
	"-d turn on debugging\n"
	, DEFAULT_LOG_FILE, DEFAULT_COOKIE_FILE_NAME, NAME, DEFAULT_GRACE_PERIOD
	, DEFAULT_LOG_ROTATION_MODULE, DEFAULT_LOG_ROTATION_FUNCTION
	);
	printf(
"Restarts due to heartbeat timeouts are limited by the restart limit (L)\n"
"and interval (I). If there are L restarts that are all less than or\n"
"equal to I seconds long, in sequence, erld will exit.\n"
"If the restart limit is 1, erld will exit after any crash that occurs\n"
"within I seconds of startup.\n"
	);
}

void version() {
	printf("%s version %s", NAME, VERSION_STRING);
}

void cleanup_terminal(int arg) {
	exit(cleanup_child(arg, NAME, erld_pid));
}

void daemonise(int arg __attribute__((unused))) {
	DEBUG("caught SIGUSR1.");
	daemonised = 1;
}
