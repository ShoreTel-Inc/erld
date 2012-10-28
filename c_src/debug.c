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

/* Debugging and error checking macros and functions */
/* Yet another set of debugging functions. Unfortunately, passing arguments through
 * "..." prevents gcc from applying it's magic printf argument checking but the
 * only other option is macro programming which is even worse... */

#include <stdlib.h>
#include <unistd.h> /* getpid */
#include <syslog.h>
#include <errno.h> /* errno */
#include <string.h> /* strerror */
#include <time.h> /* localtime */
#include <sys/time.h> /* gettimeofday */

#include "debug.h"
#include "log.h"
#include "util.h"

int debug_flag = 0;

void log_impl(int prefix, int suffix, const char *fun, const char *fmt, va_list ap);

char *create_fmt_errno_message(const char *file, int line, int e, const char *fmt, va_list ap) {
	char *msg1, *msg2;
	va_list tmp_ap;

	va_copy(tmp_ap, ap);
	msg1 = alloc_vprintf(fmt, tmp_ap);
	va_end(tmp_ap);
	if (msg1) {
		if (e)
			msg2 = alloc_printf("fatal error (file %s, line %d) in \"%s\": error %d (%s)", file, line, msg1, e, strerror(e));
		else
			msg2 = alloc_printf("fatal error (file %s, line %d): \"%s\"", file, line, msg1);
	}
	else {
		if (e)
			msg2 = alloc_printf("fatal error (file %s, line %d): error %d (%s)", file, line, e, strerror(e));
		else
			msg2 = alloc_printf("fatal error (file %s, line %d)", file, line);
	}
	free(msg1);
	return msg2;
}

void fatal_error(const char *file, int line, int e, const char *fmt, ...) {
	va_list ap;
	char *msg;

	va_start(ap, fmt);
	msg = create_fmt_errno_message(file, line, e, fmt, ap);
	va_end(ap);

	LOG_V("%s", msg);
	syslog(LOG_ERR, "%s", msg);
	free(msg);
	exit(1);
}

void debug(const char *fun, const char *fmt, ...) {
	va_list ap;

	if (!debug_flag)
		return;

	va_start(ap, fmt);
	log_impl(1, 1, fun, fmt, ap);
	va_end(ap);

}

void log_message(int prefix, int suffix, const char *fun, const char *fmt, ...) {
	va_list ap;

	va_start(ap, fmt);
	log_impl(prefix, suffix, fun, fmt, ap);
	va_end(ap);
}

void log_impl(int prefix, int suffix, const char *fun, const char *fmt, va_list ap) {
	char *msg1, *msg2;
	struct timeval now;
	struct tm *tm;

	gettimeofday(&now, 0);
	tm = localtime(&now.tv_sec);

	msg1 = alloc_vprintf(fmt, ap);
	if (prefix)
		msg2 = alloc_printf("%04d-%02d-%02d %02d:%02d:%02d.%06ld %d %s: %s%s"
		, tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday
		, tm->tm_hour, tm->tm_min, tm->tm_sec, (long) now.tv_usec
		, getpid(), fun, msg1, (suffix ? "\n" : ""));
	else
		msg2 = alloc_printf("%s%s", msg1, (suffix ? "\n" : ""));

/* Uncommenting the printf below will cause output to be displayed
   on the console until erld detaches. */
/*	printf("%s", msg2); */
	log_write(msg2, strlen(msg2));
	free(msg2);
	free(msg1);
}
