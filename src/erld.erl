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
