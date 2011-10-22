-module(bn_server).

%server api
-export([start/0,stop/0,loop/1]).

%public api
-export([echo/1,push/0]).

%TODO move to template
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
-define(SRV_NAME, ?MODULE).

%%====================================================================
%% PUBLIC API
%%====================================================================

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
echo( Msg ) ->
	{ ?SRV_NAME, ?SRV_NODE } ! { echo, self(), Msg },
	receive
		{ reply, EchoMsg } ->
			{ ok, EchoMsg };
		Other ->
			{ error, Other }
	after 500 ->
		timeout
	end.

push() ->
	{ ?SRV_NAME, ?SRV_NODE } ! { push_subscribe, self() }.

%%====================================================================
%% SERVER
%%====================================================================

start() ->
	io:fwrite( "Start server: ~p~n", [ ?SRV_NAME ]  ),
	Pid = spawn( ?SRV_NAME, loop, [ dict:new() ] ),
	register( ?SRV_NAME, Pid).

stop() ->
	?SRV_NAME ! stop.

run_report_timer( From, Num ) ->
	Delay = 5000,
	timer:send_after( Delay, { push_notification, From, Num } ).

loop(State) ->
	receive
		{ echo, From, Msg } ->
			From ! { reply, Msg },
			bn_server:loop( State );
		stop ->
			io:fwrite( "Exit normally~n" ),
			true;

		{ push_subscribe, From } ->
			io:fwrite( "srv: push_subscribed~n" ),
			run_report_timer( From, 5 )
			,bn_server:loop( State );
		{ push_notification, From, 0 } ->
			io:fwrite( "srv: push_notification: ~p~n", [0] ),
			From ! { report, 0 },
			From ! finish,
			bn_server:loop( State );
		{ push_notification, From, Num } ->
			io:fwrite( "srv: push_notification: ~p~n", [Num] ),
			From ! { report, Num },
			run_report_timer( From, Num - 1 ),
			bn_server:loop( State );

		Other ->
			io:fwrite( "Unhandled server msg~p~n", [Other] ),
			bn_server:loop( State )
	end.
