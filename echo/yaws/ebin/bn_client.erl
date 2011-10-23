-module(bn_client).

%public api
-export([echo/1,subscribe/0,deal/4]).

-include("bn_config.hrl").

%%====================================================================
%% PUBLIC API
%%====================================================================

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
dealer_response() ->
	receive
		{ reply, Response } ->
			{ ok, Response };
		{ error, ErrorDescr } ->
			{ error, ErrorDescr };
		timeout ->%TODO remove???
			timeout
	after 500 ->
		timeout
	end.

deal( Instrument, Time, Price, Amount ) ->
	{ ?SRV_NAME, ?SRV_NODE } ! { get_dealer, self(), { Instrument, Time, Price, Amount } },
	receive
		%TODO check errors also of validation time for example
		{ dealer_pid, DealerPid } ->
			DealerPid ! { self(), { Instrument, Time, Price, Amount } },
			dealer_response()
	after 500 ->
		timeout
	end.

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
			timeout
	after 500 ->
		timeout
	end.

subscribe() ->
	{ ?SRV_NAME, ?SRV_NODE } ! { push_subscribe, self() }.