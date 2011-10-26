-module(benchmark).

-export([run/0,trader/0,init/0]).

-include("bn_config.hrl").

init() ->
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

receive_report( [] ) ->
	io:fwrite( "benchmark finished~n", [] ),
	exit( normal );

receive_report( Instruments ) ->
	[ Head | Tail ] = Instruments,
	receive
		{ report, Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } ->
			Report = { Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
			io:fwrite( "Report received: ~p~n", [ Report ] )
	end,
	receive_report( Tail ).

start_traders( 0, Instruments ) ->
	bn_report:subscribe( self() ),
	receive_report( Instruments );

start_traders( Count, Instruments ) ->
	spawn_link( benchmark, trader, []),
	start_traders( Count - 1, Instruments ).

trader() ->
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
