-module(selftest).

-export([run/0]).

-include("bn_config.hrl").

run() ->
	case bn_server:start() of
		ok ->
			io:fwrite( "test_deals~n" ),
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

check_dealer_response( Resp ) ->
	case Resp of
		{ok,_Msg} ->
			io:fwrite( "New State~n" ),
			true;
		Other ->
			io:fwrite( "Error~n" ),
			throw( { error, "Deal failed", Other } )
	end.

test_random_normal_deal() ->
	Deal = bn_common:random_deal(),
	check_dealer_response( bn_server:deal( Deal ) ).

test_invalid_instrument() ->
	Deal = bn_common:random_deal(),
	{ _Instrument, DateTime, Price, Amount } = Deal,
	NewDeal = { "some_invalid_instrument", DateTime, Price, Amount },
 	case bn_server:deal( NewDeal ) of
		{error,_Msg} ->
			true;
		Other ->
			throw( { error, "Server should fail this deal", Other } )
	end.

%test_sum_of_deals_on_instument( Instrument ) ->
%	

test_deals() ->
	test_random_normal_deal(),
	test_invalid_instrument(),
	true.