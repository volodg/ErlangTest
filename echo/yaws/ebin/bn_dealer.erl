-module(bn_dealer).

-export([dealer/2]).

-include("bn_config.hrl").

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

send_report() ->
	io:fwrite( "TODO send report here~n" ),
	exit( normal ).

clients_loop( DealerInstrument, ExpirationDatetime, State ) ->
	receive
		{ From, { Instrument, Time, Price, Amount } } ->
			Instruments = sets:from_list( [ DealerInstrument ] ),

			DurationInSeconds = ?REPORT_DURATION_SEC,
			StartDatetime = datetime:addSecondToDatetime( -DurationInSeconds, ExpirationDatetime ),
			DatesSettings = { StartDatetime, ExpirationDatetime, DurationInSeconds },

			ValidDealArgs = bn_common:validate_deal_args( Instruments, DatesSettings, Instrument, Time, Price, Amount ),

			NewState = case ValidDealArgs of
				true ->
					process_deal( State, From, { Instrument, Time, Price, Amount } );
				{ error, ValidationErrorDescr } ->
					From ! { error, ValidationErrorDescr },
					State
			end,

			loop( DealerInstrument, ExpirationDatetime, NewState );
		send_report ->
			send_report();
		Other ->
			%TODO fix this if happen
			io:fwrite( "Unhandled msg in dealer (should not happen): ~p~n", [Other] ),
			loop( DealerInstrument, ExpirationDatetime, State )
	end.

%TODO validate Only date if for performance issue,
% other arguments validated by bn_server or vise versa
loop( DealerInstrument, ExpirationDatetime, State ) ->
	%TODO does it's tail recursion if callback used???
	bn_common:priority_receive( send_report, fun() ->
		clients_loop( DealerInstrument, ExpirationDatetime, State )
		end ),
	send_report().

dealer( InstrumentName, ExpirationDatetime ) ->
	%init here timer, state and etc

	%TODO if { date(), time() } > ExpirationDatetime ????
	Delay = datetime:datetime_difference_in_seconds( { date(), time() }, ExpirationDatetime ) * 1000,
	timer:send_after( Delay, send_report ),

	io:fwrite( "Start Dealer For Instrument: ~p~n", [InstrumentName] ),
	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	InitState = { open_time_todo, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	loop( InstrumentName, ExpirationDatetime, InitState ).