-module(selftest).

-export([run/0]).

-include("bn_config.hrl").

run() ->
	bn_server:start_link(),
	bn_report:start_link(),
	case catch( test_deals() ) of
		true ->
			io:fwrite( "TESTS PASSED~n" );
		{error, Msg, Details } ->
			io:fwrite( "TESTS FAILED: ~p [~p]~n", [ Msg, Details ] )
	end,
	bn_server:stop(),
	bn_report:stop().

valid_dealer_response( Resp ) ->
	case Resp of
		{ok,_Msg} ->
			true;
		Other ->
			throw( { error, "Deal failed", Other } )
	end.

invalid_dealer_response( Resp ) ->
	case Resp of
		{error,_Msg} ->
			true;
		Other ->
			throw( { error, "Server should fail this deal", Other } )
	end.

test_random_normal_deal() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument="echo3", datetime=Deal#deal.datetime, price=Deal#deal.price, amount=Deal#deal.amount},
	valid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_instrument() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument="some_invalid_instrument", datetime=Deal#deal.datetime, price=Deal#deal.price, amount=Deal#deal.amount},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_datetime_format() ->
	Deal = bn_common:random_deal(),
	InvalidDatetime = { 10, "23" },
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=InvalidDatetime, price=Deal#deal.price, amount=Deal#deal.amount},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_out_of_trading_datetime( Datetime ) ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Datetime, price=Deal#deal.price, amount=Deal#deal.amount},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_price_format() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price="Price", amount=Deal#deal.amount},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_price() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price=0, amount=Deal#deal.amount},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_amount_format() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price=Deal#deal.price, amount="Amount"},
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_amount() ->
	Deal = bn_common:random_deal(),
	NewDeal = #deal{instrument=Deal#deal.instrument, datetime=Deal#deal.datetime, price=Deal#deal.price, amount=0},
	invalid_dealer_response( bn_server:deal( NewDeal ) ).

receive_report_loop( Instrument, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount, Delay ) ->
	receive
		{ _ReportPid, #report{instrument=Instrument,
							open_price=OpenPrice,
							close_price=ClosePrice,
							min_proce=MinPrice,
							max_price=MaxPrice,
							total_amount=TotalAmount} } ->
			io:fwrite( "Report received for Instrument: ~p~n", [ Instrument ] ),
			true;
		#report{} ->
			receive_report_loop( Instrument, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount, Delay )
		after Delay ->
			throw( { error, "Have no valid report for instrument", Instrument } )
	end.

test_sum_of_deals_on_instument( Instrument ) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),

	Datetime = datetime:now_datetime(),
	Deal1 = #deal{instrument=Instrument, datetime=Datetime, price=1.4, amount=1},
	Deal2 = #deal{instrument=Instrument, datetime=Datetime, price=1.1, amount=2},
	Deal3 = #deal{instrument=Instrument, datetime=Datetime, price=1.9, amount=3},
	Deal4 = #deal{instrument=Instrument, datetime=Datetime, price=1.5, amount=4},

	valid_dealer_response( bn_server:deal( Deal1 ) ),
	valid_dealer_response( bn_server:deal( Deal2 ) ),
	valid_dealer_response( bn_server:deal( Deal3 ) ),
	valid_dealer_response( bn_server:deal( Deal4 ) ),

	bn_report:subscribe(),

	NowDatetime = datetime:now_datetime(),
	DatetimeDuration = datetime:add_second_to_datetime( ?REPORT_DURATION_SEC, NowDatetime ),
	Delay = datetime:datetime_difference_in_seconds( NowDatetime, DatetimeDuration ) * 1000 + 10,
	receive_report_loop( Instrument, 1.4, 1.5, 1.1, 1.9, 10, Delay ).

test_deals() ->
	test_random_normal_deal(),
	test_invalid_instrument(),
	test_invalid_datetime_format(),
	test_invalid_deal_price_format(),
	test_invalid_deal_price(),
	test_invalid_deal_amount_format(),
	test_invalid_deal_amount(),

	test_out_of_trading_datetime( {{2000, 11, 10},{20,20,21}} ),
	test_out_of_trading_datetime( {{3020, 11, 10},{20,20,21}} ),

	test_sum_of_deals_on_instument( "echo1" ),
	test_sum_of_deals_on_instument( "echo2" ),
	true.
