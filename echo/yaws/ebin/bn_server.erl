-module(bn_server).

%server api
-export([start/0,stop/0,loop/1]).

-include("bn_config.hrl").

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

		%get dealer for arguments
		{ get_dealer, From, { _Instrument, _Time, _Price, _Amount } } ->
			%TODO validate arguments
			%TODO separate dealer for separate instrument
			%TODO pass to daler his timeout
			DealerPid = spawn( bn_dealer, dealer, [] ),
			From ! { dealer_pid, DealerPid };

		% push notifications
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
