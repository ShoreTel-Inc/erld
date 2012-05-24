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

/* Log file handling */

#define _XOPEN_SOURCE 500 /* strdup */
#define _LARGEFILE64_SOURCE /* the log file may grow large */

#include <unistd.h> /* close */
#include <fcntl.h> /* O_ */
#include <signal.h> /* signal */
#include <syslog.h> /* syslog */
#include <stdlib.h> /* free */
#include <string.h> /* strdup */

#include "global.h"
#include "debug.h"
#include "util.h"
#include "log.h"

// All files are 'large' on mac os, so O_LARGEFILE is not defined - It's a linux extension.
#ifdef __APPLE__ 
#ifndef O_LARGEFILE
#define O_LARGEFILE 0
#endif
#endif


char *log_file_name = 0;
char *log_rotation_module = 0;
char *log_rotation_function = 0;

static int log_file_fd = -1;

static int sig_pipe = 0;

void setup_log_rotation(int pipe) {
	sig_pipe = pipe;
	set_signal(SIGHUP, handle_rotate_signal, SA_RESTART);
}

void handle_rotate_signal(int arg __attribute__((unused))) {
	char c[2] = {SIG_ROTATE_LOG, 0};
	write(sig_pipe, &c, 1);
}

void log_open() {
	const char *tmp_log_file_name = log_file_name ? log_file_name : DEFAULT_LOG_FILE;
	DEBUG_V("Using log file %s", tmp_log_file_name);
	CHECK_F(log_file_fd = open(tmp_log_file_name, O_WRONLY | O_CREAT | O_APPEND | O_LARGEFILE, 0644), "open(%s)", tmp_log_file_name);
}

void rotate_log() {
	if (log_file_fd != -1) {
		DEBUG("Log file reopened");
		syslog(LOG_INFO, "log file reopened");
		close(log_file_fd);
	}
	log_open();
}

/* NOTE: this is called during debug messages, so don't call debug from here! */
void log_write(const char *buf, size_t l) {
	if (log_file_fd == -1)
		write(2, buf, l);
	else
		write(log_file_fd, buf, l);
	/* It's tempting to add a call to fsync() here so that "logs never get
	   lost". HOWEVER, doing so causes the main (and only) thread of erld
	   to block until fsync() returns, which can be a LONG time (e.g. several
	   seconds or more). Although erld's main loop checks for a heartbeat message
	   before it checks for a timeout, it's possible that there are two messages
	   in the pipe, and the first is not the heartbeat, which will allow the
	   timeout to happen, causing an erroneous heartbeat timeout! */
}

void log_cleanup() {
	if (log_file_fd != -1)
		close(log_file_fd);
	if (log_file_name)
		free(log_file_name);
}
