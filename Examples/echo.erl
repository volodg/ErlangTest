-module(echo).
-export([start/0, loop/0]).

start() ->
	spawn(echo, loop, []).

loop() ->
	receive
		%fasdfasdfddfdsfsdf
		{From, Message} -> From ! Message,
		loop()
	end.
	
%Id = echo:start(),
%io:format( "~p~n", [ Id ! {self(), hello} ] ).
