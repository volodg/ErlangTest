-module(bn_server).

%server api
-export([start/0,stop/0,init/0,loop/3]).

-include("bn_config.hrl").

%%====================================================================
%% SERVER
%%====================================================================

start() ->
	io:fwrite( "Start server: ~p~n", [ ?SRV_NAME ]  ),
	Pid = spawn( ?SRV_NAME, init, [] ),
	register( ?SRV_NAME, Pid).

startDatetime() ->
	StartDate = date(),
	StartTime = time(),
	io:fwrite( "StartDate: {~p,~p}~n", [ StartDate, StartTime ] ),
	{ date(), time() }.

endDateTime( StartDateTime ) ->
	DuratonInSeconds = ?REPORT_DURATION_SEC,
	io:fwrite( "DuratonInSeconds: ~p~n", [DuratonInSeconds] ),
	{EndDate, EndTime} = datetime:addSecondToDatetime( DuratonInSeconds * 10, StartDateTime ),
	io:fwrite( "EndDate: {~p,~p}~n", [EndDate, EndTime] ),
	{ {EndDate, EndTime}, DuratonInSeconds }.

init() ->
	io:fwrite( "Init with instruments: ~p~n", [?INSTRUMENTS] ),

	DealerPidAndExpDateByInstrument = dict:new(),
	Instruments = sets:from_list( ?INSTRUMENTS ),

	StartDateTime = startDatetime(),
	{ EndDatetime, Duration } = endDateTime( StartDateTime ),

	loop( DealerPidAndExpDateByInstrument, Instruments, { StartDateTime, EndDatetime, Duration } ).

stop() ->
	?SRV_NAME ! stop.

%restart() ->
%	stop(),
%	receive stoped
%	start().

run_report_timer( From, Num ) ->
	Delay = 2000,
	timer:send_after( Delay, { push_notification, From, Num } ).

send_report( To, Num ) ->
	Str = integer_to_list(Num),
	To ! { report, Str }.

%validate arguments before calling this method
process_get_dealer_for_instrument( DealerPidAndExpDateByInstrument, DatesSettings, From, { Instrument, _Time, _Price, _Amount } ) ->
	{ NewDealerPidAndExpDateByInstrument, InstrumentDealerPid } = case dict:find( Instrument, DealerPidAndExpDateByInstrument ) of
		{ ok, { DealerPid, _ExpirationDate } } ->
		    %TODO check _ExpirationDate
			{ DealerPidAndExpDateByInstrument, DealerPid };
		error ->
			ExpirationDatetime = datetime:nearestExpirationDatetime( DatesSettings ),
			DealerPid = spawn( bn_dealer, dealer, [Instrument, ExpirationDatetime] ),
			NewState = dict:store( Instrument, { DealerPid, ExpirationDatetime }, DealerPidAndExpDateByInstrument ),
			{ NewState, DealerPid }
	end,
	From ! { dealer_pid, InstrumentDealerPid },
	NewDealerPidAndExpDateByInstrument.

process_get_dealer( DealerPidAndExpDateByInstrument, Instruments, DatesSettings, From, { Instrument, Time, Price, Amount } ) ->
	ValidDealArgs = bn_common:validate_deal_args( Instruments, DatesSettings, Instrument, Time, Price, Amount ),

	NewDealerPidAndExpDateByInstrument = case ValidDealArgs of
		true ->
			process_get_dealer_for_instrument( DealerPidAndExpDateByInstrument, DatesSettings, From, { Instrument, Time, Price, Amount } );
		{ error, ValidationErrorDescr } ->
			From ! { error, ValidationErrorDescr },
			DealerPidAndExpDateByInstrument
	end,
	bn_server:loop( NewDealerPidAndExpDateByInstrument, Instruments, DatesSettings ).

loop( DealerPidAndExpDateByInstrument, Instruments, DatesSettings ) ->
	receive
		{ echo, From, Msg } ->
			From ! { reply, Msg },
			bn_server:loop( DealerPidAndExpDateByInstrument, Instruments );
		stop ->
			io:fwrite( "Exit normally~n" ),
			exit(normal);

		%get dealer for arguments
		{ get_dealer, From, { Instrument, Time, Price, Amount } } ->
			process_get_dealer( DealerPidAndExpDateByInstrument, Instruments, DatesSettings, From, { Instrument, Time, Price, Amount } );

		% test push notifications
		{ push_subscribe, From } ->
			io:fwrite( "srv: push_subscribed~n" ),
			run_report_timer( From, 5 ),
			bn_server:loop( DealerPidAndExpDateByInstrument, Instruments, DatesSettings );
		{ push_notification, From, 0 } ->
			io:fwrite( "srv: push_notification: ~p~n", [0] ),
			send_report( From, 0 ),
			From ! finish,
			bn_server:loop( DealerPidAndExpDateByInstrument, Instruments, DatesSettings );
		{ push_notification, From, Num } ->
			io:fwrite( "srv: push_notification: ~p~n", [Num] ),
			send_report( From, Num ),
			run_report_timer( From, Num - 1 ),
			bn_server:loop( DealerPidAndExpDateByInstrument, Instruments, DatesSettings );

		Other ->
			io:fwrite( "Unhandled server msg~p~n", [Other] ),
			bn_server:loop( DealerPidAndExpDateByInstrument, Instruments, DatesSettings )
	end.
