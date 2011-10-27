-module(benchmark).

-export([run/0,trader/0,init/0]).

-include("bn_config.hrl").

init() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	bn_report:sync_subscribe(),
	DealRes = bn_server:deal( { Instrument, Datetime, Price, 1 } ),
	io:fwrite( "try receive report from deal ~p self:~p ~n", [DealRes,self()] ),
	%TODO fix sometimes hangups here ( may be not subscribed yet )
	receive
		{ report, _X1, _X2, _X3, _X4, _X5, _X5, _X6 } ->
			true
	end,
	io:fwrite( "Start all after report~n", [] ),

	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	start_traders(20000, ?INSTRUMENTS, []).

run() ->
	spawn_link( benchmark, init, []).

receive_report( [], AmountAsDealCount, Children ) ->
	io:fwrite( "benchmark finished with result: ~p~n", [AmountAsDealCount] ),
	lists:foreach(fun(H) -> exit(H, kill) end, Children),
	exit( normal );

receive_report( Instruments, AmountAsDealCount, Children ) ->
	[ Head | Tail ] = Instruments,
	AddAmount = receive
		{ report, Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } ->
			Report = { Head, OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
			io:fwrite( "Report received: ~p~n", [ Report ] ),
			TotalAmount
	end,
	receive_report( Tail, AmountAsDealCount + AddAmount, Children ).

start_traders( 0, Instruments, Children ) ->
	%bn_report:subscribe( self() ),
	receive_report( Instruments, 0, Children );

start_traders( Count, Instruments, Children ) ->
	ChildPid = spawn_link( benchmark, trader, []),
	start_traders( Count - 1, Instruments, [ ChildPid | Children ] ).

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
