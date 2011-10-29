%%%-------------------------------------------------------------------
%%% File    : bn_sup.erl
%%% Author  : Vladimir Gorbenko <gorbenko.vova@gmail.com>
%%% Description :
%%%
%%% Created :  29 Oct 2011 by Vladimir Gorbenko <gorbenko.vova@gmail.com>
%%%-------------------------------------------------------------------
-module(bn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	ShutdownTimeout = 2000,
	Server = {bn_server, {bn_server, start_link, []},
			permanent,ShutdownTimeout,worker,[bn_server]},
	Report = {bn_report, {bn_report, start_link, []},
			permanent,ShutdownTimeout,worker,[bn_report]},
	{ok,{{one_for_one,5,10}, [Server, Report]}}.

%%====================================================================
%% Internal functions
%%====================================================================