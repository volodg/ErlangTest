-module(bn_server).

%server api
-export([start/0,stop/0,loop/1]).

%public api
-export([echo/1,push/0,deal/4]).

%TODO move to template
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
-define(SRV_NAME, ?MODULE).

%%====================================================================
%% PUBLIC API
%%====================================================================

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
deal( Instrument, Time, Price, Amount ) ->
	io:fwrite( "Instrument: ~p~n", [Instrument] ),
	io:fwrite( "Time: ~p~n", [Time] ),
	io:fwrite( "Price: ~p~n", [Price] ),
	io:fwrite( "Amount: ~p~n", [Amount] ),
	io:fwrite( "------------------------~n" ),
	{ reply, "Good" }.

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
	Delay = 2000,
	timer:send_after( Delay, { push_notification, From, Num } ).

send_report( To, Num ) ->
	Str = integer_to_list(Num),
	To ! { report, Str }.

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
			run_report_timer( From, 5 ),
			bn_server:loop( State );
		{ push_notification, From, 0 } ->
			io:fwrite( "srv: push_notification: ~p~n", [0] ),
			send_report( From, 0 ),
			From ! finish,
			bn_server:loop( State );
		{ push_notification, From, Num } ->
			io:fwrite( "srv: push_notification: ~p~n", [Num] ),
			send_report( From, Num ),
			run_report_timer( From, Num - 1 ),
			bn_server:loop( State );

		Other ->
			io:fwrite( "Unhandled server msg~p~n", [Other] ),
			bn_server:loop( State )
	end.
