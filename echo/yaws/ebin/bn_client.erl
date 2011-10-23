-module(bn_client).

%public api
-export([echo/1,subscribe/0,deal/4]).

-include("bn_config.hrl").

%%====================================================================
%% PUBLIC API
%%====================================================================

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
deal( Instrument, Time, Price, Amount ) ->
%	{ ?SRV_NAME, ?SRV_NODE } ! { echo, self(), Msg },
	io:fwrite( "Instrument: ~p~n", [Instrument] ),
	io:fwrite( "Time: ~p~n", [Time] ),
	io:fwrite( "Price: ~p~n", [Price] ),
	io:fwrite( "Amount: ~p~n", [Amount] ),
	io:fwrite( "------------------------~n" ),
	{ reply, "Good" }.

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
echo( Msg ) ->
	{ ?SRV_NAME, ?SRV_NODE } ! { echo, self(), Msg },
	receive
		{ reply, EchoMsg } ->
			{ ok, EchoMsg };
		{ error, ErrorDescr } ->
			{ error, ErrorDescr };
		timeout ->%TODO remove???
			{ error, "srv timeout" }
	after 500 ->
		timeout
	end.

subscribe() ->
	{ ?SRV_NAME, ?SRV_NODE } ! { push_subscribe, self() }.