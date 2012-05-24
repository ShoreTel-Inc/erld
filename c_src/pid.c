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

/* Pid file handling */

#include <sys/types.h> /* open, kill */
#include <sys/stat.h> /* open */
#include <fcntl.h> /* open */
#include <stdlib.h> /* exit */
#include <stdio.h> /* printf */
#include <errno.h> /* errno */
#include <unistd.h> /* read, close, unlink, unistd */
#include <signal.h> /* kill */
#include <string.h> /* strlen */

#include "global.h"
#include "debug.h"

char *pid_file_name;

void write_pid_file() {
	int fd, l, rv;
	pid_t pid = -1;
	char buf[128];
	const char *tmp_pid_file_name;

	/* look for an existing pid file */
	tmp_pid_file_name = pid_file_name ? pid_file_name : DEFAULT_PID_FILE;
	fd = open(tmp_pid_file_name, O_RDONLY);
	if ((fd < 0) && (errno != ENOENT)) {
		perror("open");
		exit(1);
	}
	if (fd >= 0) {
		l = read(fd, buf, (sizeof buf) - 1);
		if (l > 0) {
			buf[l] = 0;
			pid = atol(buf);
			rv = kill(pid, 0);
			if (!rv || (errno == EPERM)) {
				fprintf(stderr, "Already running on pid %u\n", pid);
				exit(1);
			}
			/* printf("rv = %d, errno = %d\n", rv, errno); */
		}
		close(fd);
		fprintf(stderr, "Removing stale pid file (old pid %u).\n", pid);
		CHECK(unlink(tmp_pid_file_name));
	}

	/* write a new pid file */
	CHECK_F(fd = open(tmp_pid_file_name, O_WRONLY | O_CREAT | O_EXCL, 0666), "open(%s)", tmp_pid_file_name);
	l = snprintf(buf, sizeof buf, "%d\n", getpid());
	write(fd, buf, l);
	close(fd);
}

void remove_pid_file() {
	unlink(pid_file_name ? pid_file_name : DEFAULT_PID_FILE);
}

void pid_cleanup() {
	if (pid_file_name)
		free(pid_file_name);
}
