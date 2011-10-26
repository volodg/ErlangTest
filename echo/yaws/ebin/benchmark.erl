-module(benchmark).

-export([run/0,trader/0,init/0]).

-include("bn_config.hrl").

%TODO !!! kill childs
init() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	bn_report:subscribe( self() ),
	bn_server:deal( { Instrument, Datetime, Price, 1 } ),
	io:fwrite( "try receive~n", [] ),
	%TODO fix sometimes hangups here ( may be not subscribed yet )
	receive
		{ report, _X1, _X2, _X3, _X4, _X5, _X5, _X6 } ->
			true
	end,
	io:fwrite( "Start all after report~n", [] ),

	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	% case bn_server:start() of
	% 	ok ->
	% 		%10 000
			start_traders(20000, ?INSTRUMENTS).
	% 		bn_server:start(); %stop server
	% 	Other ->
	% 		io:fwrite( "Starting Server error: ~p~n", [Other] ),
	% 		Other
	% end.

run() ->
	spawn_link( benchmark, init, []).

receive_report( [], AmountAsDealCount ) ->
	io:fwrite( "benchmark finished with result: ~p~n", [AmountAsDealCount] ),
	exit( normal );

receive_report( Instruments, AmountAsDealCount ) ->
	[ Head | Tail ] = Instruments,
	AddAmount = receive
		{ report, Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } ->
			Report = { Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
			io:fwrite( "Report received: ~p~n", [ Report ] ),
			TotalAmount
	end,
	receive_report( Tail, AmountAsDealCount + AddAmount ).

start_traders( 0, Instruments ) ->
	%bn_report:subscribe( self() ),
	receive_report( Instruments, 0 );

start_traders( Count, Instruments ) ->
	spawn_link( benchmark, trader, []),
	start_traders( Count - 1, Instruments ).

trader() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	%io:fwrite( "Deal with Instrument: ~p~n", [Instrument] ),
	Response = bn_server:deal( { Instrument, Datetime, Price, 1 } ),
	case Response of
		{ ok, _Resp } ->
			true;
		{ error, _Descr } ->
			%io:fwrite( "Deal fail reason: ~p~n", [Descr] );
			true;
		_Other ->
			true
	end,
	trader().
