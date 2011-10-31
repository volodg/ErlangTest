%Модуль для нагрузочного тестирования
-module(benchmark).

-export([run/0,trader/0,init/0]).

-include("bn_config.hrl").

init() ->
	bn_report:subscribe(),
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price=Deal#deal.price, amount=1},
	DealRes = bn_server:deal( NewDeal ),
	io:fwrite( "try receive report from deal ~p self:~p ~n", [DealRes,self()] ),
	receive
		{ _ReportPid, #report{} } ->
			true
	end,
	io:fwrite( "Start all after report~n", [] ),

	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	start_traders(20000, ?INSTRUMENTS, []).

%запуск нагрузочных тестов
run() ->
	spawn_link( benchmark, init, []).

receive_report( [], AmountAsDealCount, Children ) ->
	io:fwrite( "benchmark finished with result: ~p~n", [AmountAsDealCount] ),
	lists:foreach(fun(H) -> exit(H, kill) end, Children),
	exit( normal );

receive_report( Instruments, AmountAsDealCount, Children ) ->
	[ HeadInstrument | Tail ] = Instruments,
	AddAmount = receive
		{ _ReportPid, #report{instrument=HeadInstrument,
							total_amount=TotalAmount} } ->
			io:fwrite( "Report received for Instrument: ~p~n", [ HeadInstrument ] ),
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
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price=Deal#deal.price, amount=1},
	Response = bn_server:deal( NewDeal ),
	case Response of
		{ ok, _Resp } ->
			true;
		{ error, _Descr } ->
			true;
		_Other ->
			true
	end,
	trader().
