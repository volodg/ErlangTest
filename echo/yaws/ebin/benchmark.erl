-module(benchmark).

-export([run/0,trader/0,init/0]).

-include("bn_config.hrl").

init() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	bn_report:sync_subscribe(),
	DealRes = bn_server:deal( { Instrument, Datetime, Price, 1 } ),
	io:fwrite( "try receive report from deal ~p self:~p ~n", [DealRes,self()] ),
	receive
		{ report, _Report } ->
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
	[ HeadInstrument | Tail ] = Instruments,
	AddAmount = receive
		{ report, { HeadInstrument, _OpenDatetime, _OpenPrice, _ClosePrice, _MinPrice, _MaxPrice, TotalAmount } } ->
			Report = { HeadInstrument, _OpenDatetime, _OpenPrice, _ClosePrice, _MinPrice, _MaxPrice, TotalAmount },
			io:fwrite( "Report received: ~p~n", [ Report ] ),
			TotalAmount
	end,
	receive_report( Tail, AmountAsDealCount + AddAmount, Children ).

start_traders( 0, Instruments, Children ) ->
	receive_report( Instruments, 0, Children );

start_traders( Count, Instruments, Children ) ->
	ChildPid = spawn_link( benchmark, trader, []),
	start_traders( Count - 1, Instruments, [ ChildPid | Children ] ).

trader() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
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
