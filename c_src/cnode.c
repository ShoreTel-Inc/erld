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

#ifndef _REENTRANT
#define _REENTRANT /* For some reason __erl_errno is undefined unless _REENTRANT is defined */
#endif

#include <unistd.h> /* read, close */
#include <fcntl.h> /* open */
#include <stdlib.h> /* malloc */
#include <string.h> /* memcpy, strerror */
#include <errno.h> /* errno */
#include <sys/socket.h> /* getpeername, inet_ntoa */
#include <netinet/in.h> /* inet_ntoa */
#include <arpa/inet.h> /* inet_ntoa */

#include "global.h"
#include "cnode.h"
#include "util.h"
#include "debug.h"

char *read_cookie(const char* cookie_file);
int open_port();

int start_cnode(const char *cookie_file, ei_cnode *ec, int *epmd_sock, int *listen_sock, const char *reg_name, const char *host_name) {
	char *cookie = 0, *default_node_name = 0, *node_name;
	short creation = 0;
	int s, port, pub, tries;
	socklen_t addr_len;
	struct sockaddr_in addr;

	cookie = read_cookie(cookie_file);
	node_name = (char *)malloc(strlen(reg_name) + strlen(host_name) + 2);
	strcpy(node_name, reg_name);
	strcat(node_name, "@");
	strcat(node_name, host_name);
	/* The thisipaddr parameter is unused at the moment (R13B01). */
	ei_connect_xinit(ec, host_name, reg_name, node_name, 0, cookie, creation);

	if (cookie)
		free(cookie);
	if (default_node_name)
		free(default_node_name);
	free(node_name);

	s = open_port();
	addr_len = sizeof addr;
	getsockname(s, (struct sockaddr *)&addr, &addr_len);
	port = ntohs(addr.sin_port);
	DEBUG_V("bound port %d", port);

	/* It's possible that epmd has been started, but hasn't yet finished starting
	 * up, so we have a few goes at connecting. Like erl, epmd goes directly into
	 * the background when started, not indicating when it's ready. */
	DEBUG("connecting to epmd");
	tries = 0;
	while (1) {
		pub = ei_publish(ec, port);
		if (pub != -1)
			break;
		if (!tries)
			fprintf(stderr, "Waiting for connection to epmd.\n");
		if (tries++ < ERLD_MAX_TRIES) {
			if (debug_flag)
				erl_err_ret("epmd connection failed, polling (attempt %d/%d)", tries, ERLD_MAX_TRIES);
			usleep(ERLD_POLL_TIME * 1000);
		}
		else {
			erl_err_ret("Unable to connect to epmd");
			return -1;
		}
	}
	*epmd_sock = pub;
	*listen_sock = s;
	return 0;
}

int accept_erlang_connection(ei_cnode *ec, int listen_sock, ErlConnect *onode) {
	int con, named;
	struct sockaddr_in addr;
	socklen_t len;

	/* Unfortunately we can't print out the address of the connection
	 * attempt if it doesn't succeed, because we don't have the socket
	 * at that stage: the accept() and more happen together inside ei_accept. */
	DEBUG("accepting connection on erlang socket");
	do {
		con = ei_accept(ec, listen_sock, onode);
	} while ((con == ERL_ERROR) && (erl_errno == EINTR));
	if (con == ERL_ERROR) {
		LOG_V("ei_accept failed (%d): %s", erl_errno, strerror(erl_errno));
		return -1;
	}
	len = sizeof addr;
	named = !getpeername(con, (struct sockaddr *)&addr, &len);
	LOG_V("connection from node %s:%d \"%s\""
	, (named ? inet_ntoa(addr.sin_addr) : "?"), (named ? ntohs(addr.sin_port) : -1), onode->nodename);
	return con;
}

char *read_cookie(const char *cookie_file) {
	char *home, *path, cookie[COOKIE_MAX_SIZE], *buf;
	ssize_t cookie_size;
	int fd;

	if (!cookie_file) {
		home = getenv("HOME");
		if (!home)
			return 0;
		path = alloc_printf("%s/%s", home, DEFAULT_COOKIE_FILE_NAME);
	} else {
		path = alloc_printf("%s", cookie_file);
	}

	CHECK_F(fd = open(path, O_RDONLY), "open(\"%s\")", path);
	CHECK_F(cookie_size = read(fd, cookie, COOKIE_MAX_SIZE), "read(\"%s\")", path);
	close(fd);
	buf = (char *)malloc(cookie_size + 1);
	memcpy(buf, cookie, cookie_size);
	buf[cookie_size] = 0;
	DEBUG_V("cookie from \"%s\" is %zu bytes", path, cookie_size);
	free(path);
	return buf;
}

int open_port() {
	int s;
	struct sockaddr_in addr;

	addr.sin_addr.s_addr = INADDR_ANY;
	addr.sin_family = AF_INET;
	addr.sin_port = 0;

	CHECK(s = socket(PF_INET, SOCK_STREAM, 0));
	CHECK(bind(s, (struct sockaddr *)&addr, sizeof addr));
	CHECK(listen(s, 5));
	return s;
}

/* Return values:
 * - -1 error
 * - 0 nothing
 * - 1 detach */
int cnode_read(int fd, char **ret_str) {
	ei_x_buff x;
	erlang_msg msg;
	int result, index, o_index, arity, type, size, ver, rv = CNODE_IGNORE;
	const char *str;
	char *s;
	char atom[MAXATOMLEN];

	ei_x_new(&x);
	/* We don't use xreceive because we're not expecting any big
	   messages and we don't want a huge term to cause our memory
	   usage to blow out and crash us. */
	/* Use a timeout of 1 millisecond even though we know we'll
	   be able to read at least some bytes, just to be sure we
	   do not get blocked (for long). Really we want to specify
	   "no timeout but don't block" to ei_receive_msg but it
	   doesn't provide that functionality. */
	result = ei_receive_msg_tmo(fd, &msg, &x, 1);

	if (result < 0) {
		LOG_V("ei_receive_msg_tmo failed (%d): %s", erl_errno, strerror(erl_errno));
		switch (erl_errno) {
			case ETIMEDOUT:
				DEBUG("ei_receive_msg_tmo: timed out.");
				break;
			case EAGAIN:
				DEBUG("ei_receive_msg_tmo: try again");
				break;
			case EMSGSIZE:
				DEBUG("ei_receive_msg_tmo: message too big");
				rv = CNODE_ERROR;
				break;
			case EIO:
				LOG_V("ei_receive_msg_tmo: IO error (%d: %s)", errno, strerror(errno));
				if (errno != EAGAIN)
					rv = CNODE_ERROR;
				break;
		}
	}
	else {
		if (result == ERL_TICK) { /* nothing to do */
			DEBUG("node tick message");
		}
		else {
			switch (msg.msgtype) {
				case ERL_SEND: str = "SEND"; break;
				case ERL_REG_SEND: str = "REG_SEND"; break;
				case ERL_LINK: str = "LINK"; break;
				case ERL_UNLINK: str = "UNLINK"; break;
				default: str = "unknown";
			}
			if (msg.msgtype == ERL_REG_SEND) {
				index = 0;

				if (ei_decode_version(x.buff, &index, &ver) == 0) {
					// DEBUG_V("data ver %d", ver);
				}

				o_index = index;

				if (ei_decode_atom(x.buff, &index, atom) == 0) {
					// DEBUG_V("atom %s", atom);
					if (!strcmp(atom, "detach")) {
						DEBUG("got detach message");
						rv = CNODE_DETACH;
					}
					else if (!strcmp(atom, "thump")) {
						rv = CNODE_THUMP;
					}
					else {
						DEBUG("message is unknown");
					}
				}
				else if (ei_decode_tuple_header(x.buff, &index, &arity) == 0) {
					if ((arity == 2)
					&& !ei_decode_atom(x.buff, &index, atom) && !strcmp(atom, "stdout")
					&& !ei_get_type(x.buff, &index, &type, &size)
					&& (type == ERL_STRING_EXT)) {
						*ret_str = (char *)malloc(size + 1);
						if (ei_decode_string(x.buff, &index, *ret_str)) {
							LOG("Failed to decode string for stdout message.");
							free(*ret_str);
							rv = CNODE_ERROR;
						}
						else
							rv = CNODE_STR;
					}
					else {
						index = o_index; // Re-set index to beginning of term.
						s = 0;
						ei_s_print_term(&s, x.buff, &index);
						LOG_V("Unknown tuple (arity %d): %s", arity, s);
						free(s);
					}
				}
				else {
					s = 0;
					ei_s_print_term(&s, x.buff, &index);
					LOG_V("Unknown message: %s", s);
					free(s);
				}
			}
			else {
				DEBUG_V("received %s message", str);
			}
		}
	}
	ei_x_free(&x);
	return rv;
}
