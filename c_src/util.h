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

#include <sys/types.h> /* pid_t */
#include <stdarg.h> /* ... */

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))

char *alloc_vprintf(const char *fmt, va_list ap);
char *alloc_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void set_cleanup_handler(void(*handler)(int));
void set_signal(int signum, void (*handler)(int), int flags);
int cleanup_child(int sig, const char *child_name, pid_t child);
void get_exit_status(int status, char **exit_status, int *rv);
