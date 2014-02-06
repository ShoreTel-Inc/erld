%	erld - A UNIX-style daemon wrapper for Erlang
%
%	The MIT License (MIT)
%	Copyright (C) 2012 ShoreTel Inc.
%
%	Permission is hereby granted, free of charge, to any person obtaining a copy
%	of this software and associated documentation files (the "Software"), to deal
%	in the Software without restriction, including without limitation the rights
%	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%	copies of the Software, and to permit persons to whom the Software is
%	furnished to do so, subject to the following conditions:
%
%	The above copyright notice and this permission notice shall be included in
%	all copies or substantial portions of the Software.
%
%	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%	THE SOFTWARE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erld_heartbeat).

-export([start_link/0, erld_heartbeat_spec/0]).
-export([send/0]).

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
			io:fwrite("Heartbeat will be late (I over slept!) - timeout was longer than expected - heartbeat sent late (late by: ~p warn time: ~p", [{late_by_sec, Late / 1000000}, ?HEARTBEAT_WARN_TIME]);
		_ ->
			ok
	end,
	send().

zombie() ->
	io:fwrite("heartbeats stopped.", []),
	receive
		start ->
			send()
	end.
