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

#include <stdlib.h> /* malloc */
#include <sys/types.h> /* */
#include <signal.h> /* kill */
#include <unistd.h> /* sleep */
#include "debug.h"
#include "options.h"

#ifdef __linux__
#include <proc/readproc.h> /* readproctab */

void slay_tree(int sig, pid_t pid, int include_children);
void slay_impl(proc_t **procs, int sig, pid_t pid, int include_children);

/*
	To have the best possible chance of cleaning up all processes, take a three phase
	approach:
	- Send a SIGQUIT ("terminal quit") to the pid and wait out the grace period.
	- If pid, or any of pid's children are still running:
	- Send a SIGSTOP to freeze them.
	- Send a SIGKILL to kill them.
*/
void slay(pid_t pid) {
	/* First ask nicely: Just send a SIGQUIT to the top process, leaving it up
	   to that process to shut down it's children. */
	slay_tree(SIGQUIT, pid, 0); /* Use slay_tree w/ no children so that we can see pid's run state */
	sleep(grace_period);     /* Give time to clean up and exit */
	slay_tree(SIGSTOP, pid, 1); /* First send SIGSTOP to freeze them, so they can't trigger SIGCHILD handlers in each other. */
	slay_tree(SIGKILL, pid, 1); /* Then do the actual killing with SIGKILL. */
}

/* Re-read the proc list each time we traverse it, because
   sending signals may cause it to change. */
void slay_tree(int sig, pid_t pid, int include_children) {
	proc_t **procs;
	int i;

	procs = readproctab(PROC_FILLSTAT);
	slay_impl(procs, sig, pid, include_children);
	for (i = 0; procs[i]; i++) {
		free(procs[i]);
	}
	free(procs);
}

void slay_impl(proc_t **procs, int sig, pid_t pid, int include_children) {
	int i, hit = 0;

	for (i = 0; procs[i]; i++) {
		if (procs[i]->tgid == pid) {
			if (procs[i]->state == 'Z') {
				LOG_V("Not killing PID %d because it's already died (in state 'Z').", pid);
			}
			else {
				LOG_V("Kill PID %d (in state '%c') with signal %d", pid, procs[i]->state, sig);
				kill(pid, sig);
			}
			hit = 1;
		}
		if (include_children && (procs[i]->ppid == pid)) {
			if (procs[i]->state == 'Z') {
				LOG_V("Not killing PID %d because it's already died (in state 'Z').", procs[i]->tgid);
			}
			else
				slay_impl(procs, sig, procs[i]->tid, include_children);
		}
	}
	if (!hit) {
		LOG_V("PID %d was missing from the proc list (while sending signal %d)", pid, sig);
	}
}
#endif

/* Not well tested, see above for comments. */
#ifdef __APPLE__
#include <libproc.h> /* proc_listpids, proc_pidinfo */
#include <sys/proc.h>

void slay_tree(int sig, pid_t pid, int include_children);
void slay_impl(pid_t *pids, int n, int sig, pid_t pid, int include_children);

void slay(pid_t pid) {
	slay_tree(SIGQUIT, pid, 0);
	sleep(grace_period);
	slay_tree(SIGSTOP, pid, 1);
	slay_tree(SIGKILL, pid, 1);
}

void slay_tree(int sig, pid_t pid, int include_children) {
	int l, n;
	pid_t *pids;

	CHECK(l = proc_listpids(PROC_ALL_PIDS, 0, 0, 0)); /* Determine max length */
	pids = (pid_t *const)malloc(l);
	CHECK(n = proc_listpids(PROC_ALL_PIDS, 0, pids, l));
	slay_impl(pids, n, sig, pid, include_children);
	free(pids);
}

void slay_impl(pid_t *pids, int n, int sig, pid_t pid, int include_children) {
	int i, hit = 0;

	DEBUG_V("slay %d with signal %d", pid, sig);
	kill(pid, sig);
	for (i = 0; i < n; i++) {
		struct proc_bsdinfo info;
		CHECK(proc_pidinfo(pids[i], PROC_PIDTBSDINFO, 0, &info, sizeof info));
		if (info.pbi_pid == pid) {
			if (info.pbi_status == SZOMB) {
				LOG_V("Not killing PID %d because it's already died (in state 'Z').", pid);
			}
			else {
				LOG_V("Kill PID %d (in state 0x%x) with signal %d", pid, info.pbi_status, sig);
				kill(pid, sig);
			}
			hit = 1;
		}
		if (include_children && (info.pbi_ppid == pid)) {
			if (info.pbi_status == SZOMB) {
				LOG_V("Not killing PID %d because it's already died (in state 'Z').", info.pbi_pid);
			}
			else
				slay_impl(pids, n, sig, info.pbi_pid, include_children);
		}
	}
	if (!hit) {
		LOG_V("PID %d was missing from the proc list (while sending signal %d)", pid, sig);
	}
}
#endif
