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

#include <erl_interface.h>
#include <ei.h>

#define CNODE_ERROR -1
#define CNODE_IGNORE 0
#define CNODE_DETACH 1
#define CNODE_THUMP 2
#define CNODE_STR 3

int start_cnode(const char *cookie, ei_cnode *ec, int *epmd_sock, int *listen_sock, const char *reg_name, const char *host_name);
int accept_erlang_connection(ei_cnode *ec, int listen_sock, ErlConnect *onode);
int cnode_read(int fd, char **ret_str);
