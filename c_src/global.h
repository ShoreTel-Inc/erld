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

/* Global constants and defaults */

#define NAME "erld" /* name of the package, base for several other defaults and messages */
#define DEFAULT_LOG_FILE ("/var/log/" NAME ".log")
#define DEFAULT_PID_FILE ("/var/run/" NAME ".pid")
#define DEFAULT_COOKIE_FILE_NAME ".erlang.cookie"
#define COOKIE_MAX_SIZE 128
#define ERLD_MAX_TRIES 30
#define ERLD_POLL_TIME 2000 /* milli-seconds */
#define RESTART_DELAY 5 /* milli-seconds to wait before restarting after a heartbeat timeout */
#define INITSCRIPT_EXIT 123 /* exit value used by init script to indicate clean exit */
#define DEFAULT_GRACE_PERIOD 5 /* seconds to wait after TSTOP before resorting to KILL */
#define DEFAULT_LOG_ROTATION_MODULE NAME
#define DEFAULT_LOG_ROTATION_FUNCTION "rotate_logs"

#define SIG_ROTATE_LOG 1
#define SIG_WAKE 2
