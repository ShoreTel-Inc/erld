%	erld - A UNIX-style daemon wrapper for Erlang
%	Copyright (C) 2012 ShoreTel Inc.
%
%	This program is free software; you can redistribute it and/or
%	modify it under the terms of the GNU General Public License
%	as published by the Free Software Foundation; either version 2
%	of the License, or (at your option) any later version.
%
%	This program is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with this program; if not, write to the Free Software
%	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erld_heartbeat).

-export([start_link/0, erld_heartbeat_spec/0]).
-export([send/0]).

-include("erlking.hrl").
-define(HEARTBEAT_TIME, 7). %% Seconds
-define(HEARTBEAT_WARN_TIME, 100000). %% microseconds after HEARTBEAT_TIME, set to 0.1 seconds

start_link() ->
	Pid = spawn_link(?MODULE, send, []),
	register(?MODULE, Pid),
	{ok, Pid}.

erld_heartbeat_spec() ->
		{ 	?MODULE,									% Id       = internal id
			{?MODULE, start_link, []},	 		% StartFun = {M, F, A}
			permanent,                       % Restart  = permanent | transient | temporary
			2000,                       		% Shutdown = brutal_kill | int() >= 0 | infinity
			worker,                     		% Type     = worker | supervisor
			[?MODULE]                   		% Modules  = [Module] | dynamic
		}.

send() ->
	erld:thump(),
	Then = now(),
	receive
		stop ->
			zombie()
	after
		?HEARTBEAT_TIME * 1000 ->
			ok
	end,
	case timer:now_diff(now(), Then) - (?HEARTBEAT_TIME * 1000000) of
		Late when Late >= ?HEARTBEAT_WARN_TIME ->
			?warning("Heartbeat will be late (I over slept!)", "timeout was longer than expected", "heartbeat sent late", [], [{late_by_sec, Late / 1000000}, ?D(?HEARTBEAT_WARN_TIME)]);
		_ ->
			ok
	end,
	send().

zombie() ->
	?debug("heartbeats stopped.", []),
	receive
		start ->
			send()
	end.
