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

% Module to interface with the erld daemon.
% At the moment this just provides some wrapper functions.
-module(erld).

-export([detach/0, thump/0, stdout/1]).
-export([check_ok/2]).

%% Indicate to erld that we're ready to detach
detach() ->
	send_to_erld(detach).

%% Send a heartbeat
thump() ->
	send_to_erld(thump).

stdout(Message) ->
	case send_to_erld({stdout, Message}) of
		false -> io:put_chars(Message);	% If we're not running within erld; just dump msg to stdio instead.
		true -> ok
	end.

send_to_erld(Term) ->
	case os:getenv("ERLD") of
		false -> false;
		Erld ->
			{erld, list_to_atom(Erld)} ! Term,
			true
	end.

check_ok({ok, _} = Result, _StatementString) ->
	Result;
check_ok(Result, StatementString) ->
	erld:stdout(lists:flatten(io_lib:format("\nFATAL: ~s failed: ~p\n", [StatementString, Result]))),
	Result.
