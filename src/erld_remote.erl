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
					{'EXIT', {shutdown, _}} -> io:fwrite("[shutdown]\n"), ok;
					% The erlang:halt call will generate this return:
					{badrpc, nodedown} -> shutdown;
					ok -> ok
					% All other cases are failures and will be caught below
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
		shutdown = remote(Node, CookieModule, init, stop, [123])
	catch
		_:_ -> erlang:halt(1)
	end,
	erlang:halt(0). %Shut down the remote control node once we're done


% Insert other remote control operations such as 'reload' in here.
