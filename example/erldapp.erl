% This module provides a very simple example of how to hook your app up to erld.

-module(erldapp).
-behaviour(application).
-behaviour(erld_app).
-behaviour(supervisor).

% application callbacks
-export([start/2, stop/1]).

% supervisor callback
-export([init/1]).

% erld_app callbacks
-export([bake_cookie/0, stop/0]).

% Standard app start function
start(_, _) ->
	% Do your normal application startup here - if any of this fails, erld will exit
	% to the console with an error code
	{ok, AppPid} = supervisor:start_link({local, erldapp_sup}, ?MODULE, []),

	% This call will detatch the erlang VM and return to the console with success (code 0).
	erld:detach(),

	% Do any startup that you want to fail in daemon mode here (that's probably none)

	{ok, AppPid}.

% Standard app stop function
stop(_) ->
	ok.

% This function reutrns the cookie that this node will use. If you want some magic, randomly
% generated cookie to be used for each iteration, or something else, generate it here.
bake_cookie() ->
	'superSecretCookie'.

% This function is called by erld to conduct a graceful shutdown of the application
% You can add your own cleanup code as required
stop() ->
	application:stop(erldapp).

% Top level supervisor
init(_) ->
	{ok, {{one_for_one, 5, 5}, [
				% The heartbeat process allows erld to detect if this erlang VM dies or locks up
				% or generally becomes unresponsive.
				erld_heartbeat:erld_heartbeat_spec()
				% Your other processes and nested supervisors go here
			]}}.
