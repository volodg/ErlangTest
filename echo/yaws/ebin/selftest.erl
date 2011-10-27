-module(selftest).

-export([run/0]).

-include("bn_config.hrl").

run() ->
	case bn_server:start() of
		ok ->
			case catch( test_deals() ) of
				true ->
					io:fwrite( "TESTS PASSED~n" );
				{error, Msg, Details } ->
					io:fwrite( "TESTS FAILED: ~p [~p]~n", [ Msg, Details ] )
			end,
			bn_server:start(); %stop server
		Other ->
			io:fwrite( "Starting Server error: ~p~n", [Other] ),
			Other
	end.

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
	{ _Instrument, Datetime, Price, Amount } = Deal,
	valid_dealer_response( bn_server:deal( { "echo3", Datetime, Price, Amount } ) ).

test_invalid_instrument() ->
	Deal = bn_common:random_deal(),
	{ _Instrument, Datetime, Price, Amount } = Deal,
	NewDeal = { "some_invalid_instrument", Datetime, Price, Amount },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_datetime_format() ->
	Deal = bn_common:random_deal(),
	{ Instrument, _Datetime, Price, Amount } = Deal,
	InvalidDatetime = { 10, "23" },
	NewDeal = { Instrument, InvalidDatetime, Price, Amount },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_out_of_trading_datetime( Datetime ) ->
	Deal = bn_common:random_deal(),
	{ Instrument, _Datetime, Price, Amount } = Deal,
	NewDeal = { Instrument, Datetime, Price, Amount },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_price_format() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, _Price, Amount } = Deal,
	NewDeal = { Instrument, Datetime, "Price", Amount },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_price() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, _Price, Amount } = Deal,
	NewDeal = { Instrument, Datetime, 0, Amount },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_amount_format() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	NewDeal = { Instrument, Datetime, Price, "Amount" },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

test_invalid_deal_amount() ->
	Deal = bn_common:random_deal(),
	{ Instrument, Datetime, Price, _Amount } = Deal,
	NewDeal = { Instrument, Datetime, Price, 0 },
 	invalid_dealer_response( bn_server:deal( NewDeal ) ).

receive_report_loop( Instrument, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount, Delay ) ->
	receive
		{ report, Instrument, _OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } ->
			io:fwrite( "Report received for Instrument: ~p~n", [ Instrument ] ),
			true;
		{ report, _OtherInstrument, _OtherOpenDatetime, _OtherOpenPrice, _OtherClosePrice, _OtherMinPrice, _OtherMaxPrice, _OtherTotalAmount } ->
			receive_report_loop( Instrument, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount, Delay )
		after Delay ->
			throw( { error, "Have no valid report for instrument", [Instrument] } )
	end.

test_sum_of_deals_on_instument( Instrument ) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),

	Datetime = datetime:now_datetime(),
	Deal1 = { Instrument, Datetime, 1.4, 1 },
	Deal2 = { Instrument, Datetime, 1.1, 2 },
	Deal3 = { Instrument, Datetime, 1.9, 3 },
	Deal4 = { Instrument, Datetime, 1.5, 4 },

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
