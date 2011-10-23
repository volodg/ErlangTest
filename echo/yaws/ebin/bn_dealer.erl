-module(bn_dealer).

-export([dealer/1]).

process_deal( State, From, { DealInstrument, DealTime, DealPrice, DealAmount } ) ->
	{ _OpenTime, OpenPrice, _ClosePrice, MinPrice, MaxPrice, TotalAmount } = State,

	io:fwrite( "In Instrument: ~p~n", [DealInstrument] ),
	io:fwrite( "In Time: ~p~n", [DealTime] ),
	io:fwrite( "In Price: ~p~n", [DealPrice] ),
	io:fwrite( "In Amount: ~p~n", [DealAmount] ),
	io:fwrite( "------------------------~n" ),

	NewState = case TotalAmount of
		0 ->
			%first deal here on instrument
			{ _OpenTime, DealPrice, DealPrice, DealPrice, DealPrice, DealAmount };
		_Other ->
			RecTotalAmount = TotalAmount + DealAmount,
			RecClosePrice = DealPrice,
			RecMinPrice = min( MinPrice, DealPrice ),
			RecMaxPrice = max( MaxPrice, DealPrice ),
			{ _OpenTime, OpenPrice, RecClosePrice, RecMinPrice, RecMaxPrice, RecTotalAmount }
	end,
	From ! { reply, "Good deal" },

	{ _NewOpenTime, NewOpenPrice, NewClosePrice, NewMinPrice, NewMaxPrice, NewTotalAmount } = NewState,
	io:fwrite( "Out NewOpenPrice: ~p~n", [NewOpenPrice] ),
	io:fwrite( "Out NewClosePrice: ~p~n", [NewClosePrice] ),
	io:fwrite( "Out NewMinPrice: ~p~n", [NewMinPrice] ),
	io:fwrite( "Out NewMaxPrice: ~p~n", [NewMaxPrice] ),
	io:fwrite( "Out NewTotalAmount: ~p~n", [NewTotalAmount] ),
	io:fwrite( "------------------------~n" ),

	NewState.

%TODO validate Instrument here
loop( State ) ->
	receive
		{ From, { Instrument, Time, Price, Amount } } ->
			loop( process_deal( State, From, { Instrument, Time, Price, Amount } ) );
		Other ->
			%TODO fix this
			io:fwrite( "Unhandled msg in dealer: ~p~n", [Other] ),
			loop( State )
	end.

dealer( InstrumentName ) ->
	%init here timer, state and etc
	io:fwrite( "Start Dealer For Instrument: ~p~n", [InstrumentName] ),
	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	InitState = { open_time_todo, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	loop( InitState ).