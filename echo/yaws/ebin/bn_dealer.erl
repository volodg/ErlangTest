-module(bn_dealer).

-export([dealer/2]).

-include("bn_config.hrl").

% log_deal( { DealInstrument, DealTime, DealPrice, DealAmount } ) ->
% 	io:fwrite( "In Instrument: ~p~n", [DealInstrument] ),
% 	io:fwrite( "In Time: ~p~n", [DealTime] ),
% 	io:fwrite( "In Price: ~p~n", [DealPrice] ),
% 	io:fwrite( "In Amount: ~p~n", [DealAmount] ),
% 	io:fwrite( "------------------------~n" ).
% 
% log_deal_rep( { _NewOpenTime, NewOpenPrice, NewClosePrice, NewMinPrice, NewMaxPrice, NewTotalAmount } ) ->
% 	io:fwrite( "Out NewOpenPrice: ~p~n", [NewOpenPrice] ),
% 	io:fwrite( "Out NewClosePrice: ~p~n", [NewClosePrice] ),
% 	io:fwrite( "Out NewMinPrice: ~p~n", [NewMinPrice] ),
% 	io:fwrite( "Out NewMaxPrice: ~p~n", [NewMaxPrice] ),
% 	io:fwrite( "Out NewTotalAmount: ~p~n", [NewTotalAmount] ),
% 	io:fwrite( "------------------------~n" ).

process_deal( State, From, { _DealInstrument, _DealTime, DealPrice, DealAmount } ) ->
	io:fwrite( "try process_deal: ~p~n", [{ _DealInstrument, _DealTime, DealPrice, DealAmount }] ),
	{ _OpenTime, OpenPrice, _ClosePrice, MinPrice, MaxPrice, TotalAmount } = State,

	% log_deal( { DealInstrument, DealTime, DealPrice, DealAmount } ),

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

	% log_deal_rep( NewState ),

	NewState.

send_report( DealerInstrument, State ) ->
	{ OpenTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } = State,
	case TotalAmount of
		TotalAmount when TotalAmount > 0 ->
			bn_report:notify( { report, DealerInstrument, OpenTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } );
		_Other ->
			ignore
	end,
	exit( normal ).

clients_loop( DealerInstrument, ExpirationDatetime, State ) ->
	receive
		{ From, Deal } ->
			DurationInSeconds = ?REPORT_DURATION_SEC,
			StartDatetime = datetime:add_second_to_datetime( -DurationInSeconds, ExpirationDatetime ),
			DatesSettings = { StartDatetime, ExpirationDatetime, DurationInSeconds },

			ValidDealArgs = bn_common:validate_deal_args( [ DealerInstrument ], DatesSettings, Deal ),

			NewState = case ValidDealArgs of
				true ->
					process_deal( State, From, Deal );
				{ error, ValidationErrorDescr } ->
					From ! { error, ValidationErrorDescr },
					State
			end,

			loop( DealerInstrument, ExpirationDatetime, NewState );
		send_report ->
			send_report( DealerInstrument, State );
		Other ->
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
	%TODO remove this or use NewState instead of State ( test performance before ) 
	send_report( DealerInstrument, State ).

dealer( InstrumentName, ExpirationDatetime ) ->
	%init here timer, state and etc

	%TODO if { date(), time() } > ExpirationDatetime ????
	Delay = datetime:datetime_difference_in_seconds( { date(), time() }, ExpirationDatetime ) * 1000,
	timer:send_after( Delay, send_report ),

	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	OpenDateTime = datetime:add_second_to_datetime( -?REPORT_DURATION_SEC, ExpirationDatetime ),
	InitState = { OpenDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	loop( InstrumentName, ExpirationDatetime, InitState ).
