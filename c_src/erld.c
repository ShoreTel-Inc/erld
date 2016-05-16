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

#define _XOPEN_SOURCE 500 /* required for pty related functions, strdup, vsnprintf etc. */
#define _DARWIN_C_SOURCE    /* required for timer* funcs on mac */
#include <sys/types.h> /* pid_t */
#include <sys/select.h> /* select */
#include <sys/utsname.h> /* uname */
#include <syslog.h> /* openlog, syslog */
#include <unistd.h> /* read, write, close, gethostname, sleep */
#include <fcntl.h> /* open */
#include <signal.h> /* kill */
#include <stdlib.h> /* ptsname */
#include <limits.h> /* HOST_NAME_MAX */
#include <netdb.h> /* gethostbyname */
#include <sys/time.h> /* gettimeofday */
#include <time.h> /* difftime */
#include <errno.h> /* errno */
#include <string.h> /* strerror */
#include <ctype.h> /* toupper */
extern int h_errno;

#include "options.h"
#include "global.h"
#include "pid.h"
#include "log.h"
#include "cnode.h"
#include "util.h"
#include "slay.h"
#include "debug.h"


// HOST_NAME_MAX is not defined on many Unices these days - it's archaic and deprecated.
#ifdef __APPLE__
#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 255
#endif
#endif

#define BUF_LEN 1024

char *node_name = 0;

static pid_t erl_pid = -1;
static int detached = 0;
static int killed = 0;
static int exited = 0;

/* Accessed by signal handlers so must be global: */
static int sig_pipe[2];

int erld_main_loop(char *const argv[], int epmd_sock, int listen_sock, ei_cnode *ec, int log_pipe_fd);
pid_t fork_exec_erl(char *const argv[], int epmd_sock, int listen_sock, int *m_pty);
int cleanup_erld();
void handle_kill(int arg);
void handle_exit(int arg);

/* This is what you might call the real main function of erld.
 * It handles the following things:
 * - Forking and running the real erl as a child.
 * - The virtual terminal that contains erl.
 * - Untill we detach, it echos the output of erl to the terminal.
 * - It's an Erlang C-node (so it can talk to erl).
 * - Writing the log file (if configured).
 * - Handles SIGHUP log rotation.
 */
int erld_main(char *cookie, char *const argv[]) {
	char hostname[HOST_NAME_MAX+1], *tmp_node_name, *erld_env, *tmp_name;
	struct hostent *host;
	ei_cnode ec;
	int epmd_sock, listen_sock;
	int restart, restarts = 0;
	struct timeval now, last_start, dt;
	size_t i;

	DEBUG_V("foreground=%d", foreground);

	if (pid_file_name)
		write_pid_file();

	/* To determine our local fully qualified domain name (FQDN) we follow the
	 * same logic as hostname(1). This is important because this program must
	 * interact well with shell scripts that will call hostname(1) to get the FQDN. */

	CHECK(gethostname(hostname, HOST_NAME_MAX));
	DEBUG_V("hostname %s", hostname);
	hostname[HOST_NAME_MAX] = 0; /* POSIX doesn't guarantee a terminator */
	do {
		host = gethostbyname(hostname);
		if (host)
			break;
		if(h_errno == TRY_AGAIN) {
			fprintf(stderr, "gethostbyname(%s) returned TRY_AGAIN. Trying again in a second.\n", hostname);
			sleep(1);
			continue;
		}
		fatal_error(__RELFILE__, __LINE__, errno, "Hostname lookup of %s failed! h_errno %d", hostname, h_errno);
	} while (1);
	DEBUG_V("FQDN %s", host->h_name);

	tmp_node_name = node_name ? node_name : alloc_printf(NAME "-%d", getpid());
	tmp_name = strdup(NAME);
	for (i = 0; i < strlen(tmp_name); i++)
		tmp_name[i] = toupper(tmp_name[i]);

	erld_env = alloc_printf("%s=%s@%s", tmp_name, tmp_node_name, host->h_name);
	DEBUG_V("export %s", erld_env);
	putenv(erld_env);

	free(tmp_name);

	/* Start C-node */
	if (start_cnode(cookie, &ec, &epmd_sock, &listen_sock, tmp_node_name, host->h_name) == -1) {
		DEBUG("start_cnode failed.");
		return 1;
	}

	if (!node_name)
		free(tmp_node_name);

	/* master */
	openlog(NAME, 0, LOG_DAEMON);

	// Setup a pipe that signal handlers can write
	// to to wake us up and trigger things.
	CHECK(pipe(sig_pipe));
	fcntl(sig_pipe[0], F_SETFL, O_NONBLOCK);
	setup_log_rotation(sig_pipe[1]);
	log_open();

	syslog(LOG_INFO, "start");
	DEBUG_V("restart_limit: %d, restart_interval: %ld.%06ld"
	, restart_limit, restart_interval.tv_sec, (long) restart_interval.tv_usec);

	do {
		gettimeofday(&last_start, 0);
		restart = erld_main_loop(argv, epmd_sock, listen_sock, &ec, sig_pipe[0]);
		if (restart && !detached) {
			fprintf(stderr, "Erlang exited before detaching, not trying to restart it.\n");
			LOG("Erlang exited before detaching, not trying to restart it.");
			restart = 0;
		}
		if (restart) {
			gettimeofday(&now, 0);
			timersub(&now, &last_start, &dt);
			if ((restart_limit > 0) && (timerisset(&restart_interval))) {
				if (timercmp(&dt, &restart_interval, <=)) {
					restarts++;
					LOG_V("Run time %ld.%06ld sec <= interval %ld.%06ld, incrementing restart count to %d (limit %d)."
					, dt.tv_sec, (long) dt.tv_usec, restart_interval.tv_sec, (long) restart_interval.tv_usec
					, restarts, restart_limit);
					if (restarts >= restart_limit) {
						syslog(LOG_WARNING, "Restart limit reached, bailing out.");
						LOG("Restart limit reached, bailing out.");
						restart = 0;
					}
				}
				else {
					LOG_V("Run time %ld.%06ld sec > interval %ld.%06ld sec, clearing restart count."
					, dt.tv_sec, (long) dt.tv_usec, restart_interval.tv_sec, (long) restart_interval.tv_usec);
					restarts = 0;
				}
			}
			if (restart) {
				DEBUG("Sleeping for restart delay.");
				sleep(RESTART_DELAY);
				syslog(LOG_INFO, "Restarting Erlang.");
				LOG("Restarting Erlang.");
				killed = exited = 0;
			}
		}
	} while(restart);

	syslog(LOG_INFO, "stop");

	remove_pid_file();

	free(erld_env);
	if (cookie)
		free(cookie);
	DEBUG_V("%s exiting, code %d", NAME, !detached);

	pid_cleanup();
	log_cleanup();
	return !detached;
}

int erld_main_loop(char *const argv[], int epmd_sock, int listen_sock, ei_cnode *ec, int sig_pipe_fd) {
	int erlang_sock = -1, tmp_erlang_sock;
	int m_pty, max_fd, n, l, i;
	char buf[BUF_LEN];
	ErlConnect onode;
	fd_set r_fds;
	struct timeval tv, *tvp;
	struct timeval now, last_hb, dt, soon;
	int restart = 0;
	int null_fd;
	char *p, *q;
	int at_eol = 1;
	char *ret_str = 0;

	/* Fork child to run real erl */
	erl_pid = fork_exec_erl(argv, epmd_sock, listen_sock, &m_pty);
	DEBUG_V("start erl_pid = %d", erl_pid);

	timerclear(&soon);
	soon.tv_usec = 1;
	gettimeofday(&last_hb, 0);

	/* main loop: 
	 * - copy or throw away virtual terminal output to the console
	 * - accept an Erlang connection
	 * - handle Erlang messages on an Erlang connection */
	do {
		FD_ZERO(&r_fds);
		max_fd = 0;
		if (m_pty >= 0) {
			FD_SET(m_pty, &r_fds);
			max_fd = MAX(max_fd, m_pty);
		}
		FD_SET(sig_pipe_fd, &r_fds);
		max_fd = MAX(max_fd, sig_pipe_fd);
		if (listen_sock >= 0) {
			FD_SET(listen_sock, &r_fds);
			max_fd = MAX(max_fd, listen_sock);
		}
		if (erlang_sock >= 0) {
			FD_SET(erlang_sock, &r_fds);
			max_fd = MAX(max_fd, erlang_sock);
		}
		if (timerisset(&heartbeat_timeout)) {
			/* If using heartbeats, set the select timeout to the
			  remaining time before the heartbeat timer expires. */
			gettimeofday(&now, 0);
			timersub(&now, &last_hb, &dt);
			timersub(&heartbeat_timeout, &dt, &tv);
			DEBUG_V("last heartbeat was %ld.%06ld seconds ago, timeout in %ld.%06ld seconds."
			, dt.tv_sec, (long) dt.tv_usec, tv.tv_sec, (long)tv.tv_usec);
			if (timercmp(&tv, &soon, <)) {
				/* More paranoia: Never have select timeout immediately because if something
				   is wrong it may lead to hard-looping at 100% CPU. Just set the timeout
				   to something fairly small instead. */
				DEBUG_V("heartbeat timeout very soon, setting timeout to %ld.%06ld seconds."
				, soon.tv_sec, (long) soon.tv_usec);
				timerclear(&tv);
				tv.tv_sec = 1;
			}
			tvp = &tv;
		}
		else /* If not using heartbeats, don't time out */
			tvp = 0;
		n = select(max_fd + 1, &r_fds, 0, 0, tvp);
		if ((n < 0) && (errno != EINTR)) {
			FATAL_ERROR(errno, "select()");
		}
		if (n > 0) {
			if ((m_pty >= 0) && FD_ISSET(m_pty, &r_fds)) {
				l = read(m_pty, buf, (sizeof buf) - 1);
				if (l <= 0) {
					DEBUG("pty closed, will exit shortly.");
					m_pty = -1;
				}
				else {
					/* find all the lines in the buffer and use log() to write them to the log */
					/* TODO: it would be better to fully line buffer this so that big lines that */
					/* are broken across read() calls were handled correctly, but I just don't */
					/* have the time at the moment... */
					/* Better than before, but doesn't handle a \r\n sequence broken
					   across different reads. */
					buf[l] = 0;
					p = buf;
					while ((q = strstr(p, "\r\n"))) {
						/* q points to the end of a complete line */
						*q = 0; /* null terminate this line */
						log_message(at_eol, 1, "erl", "%s", p);
						at_eol = 1;
						p = q + 2; /* 2 == length of separator, \r\n */
					}
					if (strlen(p)) {
						/* there was an unterminated line at the end */
						log_message(at_eol, 0, "erl", "%s", p);
						at_eol = 0;
					}
				}
			}
			if (FD_ISSET(sig_pipe_fd, &r_fds)) {
				CHECK(l = read(sig_pipe_fd, buf, sizeof buf));

				if (!l) {
					FATAL_ERROR(errno, "Internal wakeup pipe was closed!");
				}

				for (i = 0; i < l; i++) {
					switch (buf[i]) {
						case SIG_ROTATE_LOG:
							DEBUG("Log rotation signal received");
							rotate_log();

							/* Send the log rotate message to the erlang node */
							if (erlang_sock >= 0) {
								ei_x_buff result, args;
								ei_x_new(&result);
								ei_x_new(&args);
								ei_x_encode_empty_list(&args);
								if (ei_rpc(ec, erlang_sock
								, (char*)(log_rotation_module ? log_rotation_module : DEFAULT_LOG_ROTATION_MODULE)
								, (char*)(log_rotation_function ? log_rotation_function : DEFAULT_LOG_ROTATION_FUNCTION)
								, args.buff, args.index, &result) == -1)
									LOG_V("Error %d when trying to rotate erlking logs: %s", erl_errno, strerror(errno));
								ei_x_free(&args);
								ei_x_free(&result);
							}
							break;
						case SIG_WAKE:
							/* Often this won't be seen but if the SIGCHLD (or other signal)
							   comes in between where the loop exit tests (killed and exited)
							   and select goes to sleep, this prevents a deadlock. */
							DEBUG("Pipe wakup.");
							break;
						default:
							LOG_V("Bad command in sig pipe: %c (char 0x%02x)", buf[i], buf[i]);
					}
				}
			}
			if ((listen_sock >= 0) && FD_ISSET(listen_sock, &r_fds)) {
				if ((tmp_erlang_sock = accept_erlang_connection(ec, listen_sock, &onode)) == -1) {
					if (erlang_sock == -1) {
						LOG("Failed to accept first connection from erl node: exit.");
						break; // couldn't connect and no previous one to fall back to... bail out.
					}
					else {
						LOG("Failed to accept a new connection from the erl node, falling back to previous connection.");
					}
				}
				else {
					if (erlang_sock >= 0) {
						LOG_V("closing previous erlang connection on fd %d.", erlang_sock);
						close(erlang_sock);
					}
					erlang_sock = tmp_erlang_sock;
				}
			}
			if ((erlang_sock >= 0) && FD_ISSET(erlang_sock, &r_fds)) {
				switch (cnode_read(erlang_sock, &ret_str)) {
					case CNODE_ERROR:
						close(erlang_sock);
						erlang_sock = -1;
						DEBUG("Erlang socket closed.");
						break;
					case CNODE_IGNORE:
						DEBUG("Erlang CNODE_IGNORE.");
						break;
					case CNODE_DETACH:
						if (detached)
							DEBUG("detach message received but already detached, ignored.");
						else {
							detached = 1;
							if (foreground)
								DEBUG("detach message received (but staying in the foreground)");
							else {
								DEBUG("detaching...");
								/* We can't let these fds be reused, because erl is going to assume
								 * that they have the normal meaning, and yet we must close them to
								 * complete detaching from the terminal. So, we dup them closed
								 * with a fd that points to dev null */
								CHECK(null_fd = open("/dev/null", O_RDONLY));
								dup2(null_fd, 0);
								close(null_fd);
								CHECK(null_fd = open("/dev/null", O_WRONLY));
								dup2(null_fd, 1);
								dup2(null_fd, 2);
								close(null_fd);
								kill(getppid(), SIGUSR1);
							}
						}
						break;
					case CNODE_THUMP:
						if (!restart && timerisset(&heartbeat_timeout)) {
							gettimeofday(&now, 0);
							timersub(&now, &last_hb, &dt);
							if (timerisset(&heartbeat_warn) && timercmp(&dt, &heartbeat_warn, >=)) {
								LOG_V("THUMP: WARNING heartbeat was late: %ld.%06ld seconds (warn at %ld.%06ld, timeout at %ld.%06ld)."
								, dt.tv_sec, (long) dt.tv_usec, heartbeat_warn.tv_sec, (long) heartbeat_warn.tv_usec
								, heartbeat_timeout.tv_sec, (long) heartbeat_timeout.tv_usec);
								syslog(LOG_WARNING, "%s late heartbeat (%ld.%06ld seconds).", NAME, dt.tv_sec, (long) dt.tv_usec);
							}
							else {
								DEBUG_V("THUMP: heartbeat took %ld.%06ld seconds (timeout at %ld.%06ld)."
								, dt.tv_sec, (long) dt.tv_usec
								, heartbeat_timeout.tv_sec, (long) heartbeat_timeout.tv_usec);
							}
							last_hb = now;
						}
						else {
							DEBUG("CNODE_THUMP");
						}
						break;
					case CNODE_STR:
						/* NOTE: The string is written verbatim to standard out so that
						 * fragments of a long line can be written a piece at a time (with
						 * each piece being flushed), but the copy written to the log file
						 * has an EOL appended to maintain the log file format. */
						log_message(1, 1, "stdout", "%s", ret_str);
						if (!detached) {
							printf("%s", ret_str);
							fflush(stdout);
						}
						free(ret_str);
						ret_str = 0;
				}
			}
		}
		if (!restart && timerisset(&heartbeat_timeout)) {
			gettimeofday(&now, 0);
			timersub(&now, &last_hb, &dt);
			if (timercmp(&dt, &heartbeat_timeout, >=)) {
				LOG_V("heartbeat timeout (%ld.%06ld seconds), killing and restarting."
				, dt.tv_sec, (long) dt.tv_usec);
				syslog(LOG_WARNING, "%s heartbeat timeout, killing and restarting.", NAME);
				slay(erl_pid);
				restart = 1;
			}
		}
	}
	while (!killed && !exited); /* Note: several breaks can exit the loop */

	if (killed) {
		DEBUG_V("%s killed by signal %d", NAME, killed);
	}
	if (exited) {
		DEBUG_V("erl has exited (%d)", exited);
	}

		/* cleanup */
	if (!detached)
		DEBUG("Lost connection to erl before receving a detach message.\n");
	if (m_pty >= 0)
		close(m_pty);
	return cleanup_erld();
}

/* This is the process that execs erld. Not much to see here except a
 * normal pty pipe and redirect setup. */
pid_t fork_exec_erl(char *const argv[], int listen_sock, int epmd_sock, int *m_pty) {
	pid_t pid;
	int s_pty;
	int se, fd_flags;
	char *sname;

	/* child process setup */
	CHECK(*m_pty = open("/dev/ptmx", O_RDWR));
	sname = ptsname(*m_pty);
	CHECK(grantpt(*m_pty));
	CHECK(unlockpt(*m_pty));
	CHECK(pid = fork());

	if (pid) {
		/* signal handlers for child cleanup */
		set_cleanup_handler(handle_kill);
		set_signal(SIGCHLD, handle_exit, 0);
		return pid;
	}

	/* child */
	set_signal(SIGCHLD, SIG_DFL, 0);

	/* Close parent descriptors */
	log_cleanup();
	close(epmd_sock);
	close(listen_sock);
	close(*m_pty);

	/* Chdir to "/" so that we don't hold a the current directory open */
	// I'm removing this for the moment because it loses CWD data set in the
	// init script which can be required in certain setups.
	//CHECK(chdir("/"));

	/* Start the Unix pty dance... */
	setsid();
	CHECK_F(s_pty = open(sname, O_RDWR), "open(%s)", sname);
	// Set up SE as a temporary standard error descriptor.
	// We'll send errors from exec() to this descriptor and
	// mark it as "close on exec" so that if exec() succeeds
	// it will be automatically closed.
	CHECK(se = dup(2));
	fd_flags = fcntl(se, F_GETFD);
	fd_flags |= FD_CLOEXEC;
	CHECK(fcntl(se, F_SETFD, fd_flags));
	CHECK(fcntl(2, F_SETFD, FD_CLOEXEC));
	dup2(s_pty, 0);
	dup2(s_pty, 1);
	dup2(s_pty, 2);
	execvp(argv[0], argv);
	// If exec failed, restore standard error:
	if (!detached)
		dup2(se, 2);
	FATAL_ERROR_V(errno, "exec(%s)", argv[0]);
	return 0; /* not reached, fatal_error does not return */
}

/* Return 1 if erl should be restarted.
 * Aim: Restart after any signal, except INT (e.g. kill or ^C) or TERM (e.g. kill -9)
 * Restart on any exit code except the magic one used by the init script to indicate
 * that the user really wants it to stop.
 */
int cleanup_erld() {
	int exit_code;

	exit_code = cleanup_child(killed, "erl", erl_pid);
	erl_pid = -1;
	if (killed == SIGINT || killed == SIGTERM)
		return 0;
	if (exited && exit_code == INITSCRIPT_EXIT)
		return 0;
	return 1;
}

void handle_kill(int arg) {
	char c = SIG_WAKE;

	killed = arg;
	write(sig_pipe[1], &c, sizeof c);
}

void handle_exit(int arg) {
	char c = SIG_WAKE;

	exited = arg;
	write(sig_pipe[1], &c, sizeof c);
}

