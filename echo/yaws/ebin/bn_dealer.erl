-module(bn_dealer).

-export([dealer/0]).

%Instrument, Time, Price, Amount
loop() ->
	receive
		{ From, { Instrument, Time, Price, Amount } } ->
			io:fwrite( "Instrument: ~p~n", [Instrument] ),
			io:fwrite( "Time: ~p~n", [Time] ),
			io:fwrite( "Price: ~p~n", [Price] ),
			io:fwrite( "Amount: ~p~n", [Amount] ),
			io:fwrite( "------------------------~n" ),
			From ! { reply, "Good" };
		Other ->
			%TODO fix this
			io:fwrite( "Unhandled msg: ~p~n", [Other] )
	end.

dealer() ->
	%init here timer, state and etc
	loop().