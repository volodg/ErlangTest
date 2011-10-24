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
	%TODO change 10000
	{EndDate, EndTime} = datetime:addSecondToDatetime( DuratonInSeconds * 10000, StartDateTime ),
	io:fwrite( "EndDate: {~p,~p}~n", [EndDate, EndTime] ),
	{ {EndDate, EndTime}, DuratonInSeconds }.

init() ->
	io:fwrite( "Init with instruments: ~p~n", [?INSTRUMENTS] ),

	bn_report:start_link(),

	State = dict:store( dealer_info_by_instrument, dict:new(), dict:new() ),

	Instruments = sets:from_list( ?INSTRUMENTS ),

	StartDateTime = startDatetime(),
	{ EndDatetime, Duration } = endDateTime( StartDateTime ),

	loop( State, Instruments, { StartDateTime, EndDatetime, Duration } ).

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

run_new_dealer_for_instrument( State, Instrument, DatesSettings ) ->
	ExpirationDatetime = datetime:nearestExpirationDatetime( DatesSettings ),
	DealerPid = spawn( bn_dealer, dealer, [Instrument, ExpirationDatetime] ),
	NewState = set_dealer_info( State, Instrument, { DealerPid, ExpirationDatetime } ),
	{ NewState, DealerPid }.

%validate arguments before calling this method
process_get_dealer_for_instrument( State, DatesSettings, From, { Instrument, _Time, _Price, _Amount } ) ->
	{ NewState, InstrumentDealerPid } = case find_dealer_info( State, Instrument ) of
		{ ok, { DealerPid, ExpirationDate } } ->
			NearestNextStartDate = datetime:nearestExpirationDatetime( DatesSettings ),
			ExpirationDateExpared = datetime:datetimeEarlierThanDatetime( ExpirationDate, NearestNextStartDate ),
		    case ExpirationDateExpared of
				true ->
					run_new_dealer_for_instrument( State, Instrument, DatesSettings );
				false ->
					{ State, DealerPid }
			end;
		error ->
			run_new_dealer_for_instrument( State, Instrument, DatesSettings )
	end,
	From ! { dealer_pid, InstrumentDealerPid },
	NewState.

process_get_dealer( State, Instruments, DatesSettings, From, { Instrument, Time, Price, Amount } ) ->
	ValidDealArgs = bn_common:validate_deal_args( Instruments, DatesSettings, Instrument, Time, Price, Amount ),

	NewState = case ValidDealArgs of
		true ->
			process_get_dealer_for_instrument( State, DatesSettings, From, { Instrument, Time, Price, Amount } );
		{ error, ValidationErrorDescr } ->
			From ! { error, ValidationErrorDescr },
			State
	end,
	bn_server:loop( NewState, Instruments, DatesSettings ).

%returns { ok, { DealerPid, ExpirationDate } } or error
find_dealer_info( State, Instrument ) ->
	%io:fwrite( "Print State: ~p~n", [State] ),
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	dict:find( Instrument, DealerPidAndExpDateByInstrument ).

%returns NewState
set_dealer_info( State, Instrument, DealerInfo ) ->
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	NewDealerPidAndExpDateByInstrument = dict:store( Instrument, DealerInfo, DealerPidAndExpDateByInstrument ),
	dict:store( dealer_info_by_instrument, NewDealerPidAndExpDateByInstrument, State ).

loop( State, Instruments, DatesSettings ) ->
	receive
		{ echo, From, Msg } ->
			From ! { reply, Msg },
			bn_server:loop( State, Instruments, DatesSettings );
		stop ->
			io:fwrite( "Exit normally~n" ),
			exit(normal);

		%get dealer for arguments
		{ get_dealer, From, { Instrument, Time, Price, Amount } } ->
			process_get_dealer( State, Instruments, DatesSettings, From, { Instrument, Time, Price, Amount } );

		% test push notifications
		{ push_subscribe, From } ->
			io:fwrite( "srv: push_subscribed~n" ),
			run_report_timer( From, 5 ),
			bn_server:loop( State, Instruments, DatesSettings );
		{ push_notification, From, 0 } ->
			io:fwrite( "srv: push_notification: ~p~n", [0] ),
			send_report( From, 0 ),
			From ! finish,
			bn_server:loop( State, Instruments, DatesSettings );
		{ push_notification, From, Num } ->
			io:fwrite( "srv: push_notification: ~p~n", [Num] ),
			send_report( From, Num ),
			run_report_timer( From, Num - 1 ),
			bn_server:loop( State, Instruments, DatesSettings );

		Other ->
			io:fwrite( "Unhandled server msg~p~n", [Other] ),
			bn_server:loop( State, Instruments, DatesSettings )
	end.
