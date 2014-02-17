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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module allows external remote control of erld-based erlang nodes.
% An application that is going to be controlled must implement the erld_app
% behaviour to provide access to the node's cookie and 'stop' operation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erld_remote).
-export([stop/2]).

% General-purpose remote execution function - sets up local node,
% connection to the remote node, issues the rpc:call and gets the
% response.
remote(NodeName, CookieModule, Module, Function, Arguments) ->
	net_kernel:start([list_to_atom("erld_remote_" ++ os:getpid())]),
	N = atom_to_list(node()),
	Node = list_to_atom(NodeName ++ "@" ++ string:sub_word(N, 2, $@)),
	Cookie = CookieModule:bake_cookie(),
	erlang:set_cookie(node(), Cookie),
	case net_adm:ping(Node) of
		pong ->
			try
				case rpc:call(Node, Module, Function, Arguments) of
					{'EXIT', {shutdown, _}} ->
						io:fwrite("[shutdown]\n"),
						ok;
					{badrpc, nodedown} ->
						% The erlang:halt call will generate this return value
						shutdown;
					ok ->
						% All other cases are failures and will be caught here
						ok
				end
			catch
				A:B ->
					io:fwrite("Operation failed, exception: ~p:~p\n", [A, B])
			end;
		pang ->
			io:fwrite("Service does not appear to be running.\n")
	end.

% Shuts down the remote application and stops the remote node.
-spec stop(string(), atom()) -> no_return().
stop(Node, CookieModule) ->
	try
		% 123 is the erld magic number to indicate a clean exit requested from the init script
		ok = remote(Node, CookieModule, init, stop, [123])
	catch
		_:_ -> erlang:halt(1)
	end,
	erlang:halt(0). %Shut down the remote control node once we're done


% Insert other remote control operations such as 'reload' in here.
