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

/* See source file for description */

#include <stdarg.h>
#include <stdio.h>
#include <errno.h>

extern int debug_flag;
extern int ei_tracelevel; // Hook into the ei library debug

#define CHECK(STMT) if ((STMT) == -1) { fatal_error(__RELFILE__, __LINE__, errno, "%s", #STMT); }
#define CHECK_F(STMT, FMT, ...) if ((STMT) == -1) { fatal_error(__RELFILE__, __LINE__, errno, FMT, __VA_ARGS__); }
#define FATAL_ERROR(ERRNO, FMT) fatal_error(__RELFILE__, __LINE__, ERRNO, FMT)
#define FATAL_ERROR_V(ERRNO, FMT, ...) fatal_error(__RELFILE__, __LINE__, ERRNO, FMT, __VA_ARGS__)

#define DEBUG(FMT) debug(__FUNCTION__, FMT)
#define DEBUG_V(FMT, ...) debug(__FUNCTION__, FMT, __VA_ARGS__)

#define LOG(FMT) log_message(1, 1, __FUNCTION__, FMT)
#define LOG_V(FMT, ...) log_message(1, 1, __FUNCTION__, FMT, __VA_ARGS__)

void fatal_error(const char *file, int line, int e, const char *fmt, ...) __attribute__((format(printf, 4, 5)));
void log_message(int prefix, int suffix, const char *fun, const char *fmt, ...) __attribute__((format(printf, 4, 5)));
void debug(const char *fun, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
