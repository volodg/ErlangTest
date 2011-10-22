-module(bn_server).

%server api
-export([start/0,stop/0,loop/1]).

%public api
-export([echo/1]).

%TODO move to template
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
-define(SRV_NAME, ?MODULE).

%%====================================================================
%% PUBLIC API
%%====================================================================

%returns { ok, Msg }, timeout or { error, ErrorDescr }
%pass Node name as arg
echo( Msg ) ->
	{ ?SRV_NAME, ?SRV_NODE } ! { echo, self(), Msg },
	receive
		{ reply, EchoMsg } ->
			{ ok, EchoMsg };
		Other ->
			{ error, Other }
	after 500 ->
		timeout
	end.

%%====================================================================
%% SERVER
%%====================================================================

start() ->
	io:fwrite( "Start server: ~p~n", [ ?SRV_NAME ]  ),
	Pid = spawn( ?SRV_NAME, loop, [ dict:new() ] ),
	register( ?SRV_NAME, Pid).

stop() ->
	?SRV_NAME ! stop.

loop(State) ->
	receive
		{ echo, From, Msg } ->
			From ! { reply, Msg },
			bn_server:loop( State );
		stop ->
			io:fwrite( "Exit normally~n" ),
			true;
		Other ->
			io:write( "Unhandled server msg~p~n", [Other] ),
			bn_server:loop( State )
	end.
