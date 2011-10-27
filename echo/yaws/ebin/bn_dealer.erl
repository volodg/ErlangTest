-module(bn_dealer).

-export([dealer/2]).

-include("bn_config.hrl").

process_deal( State, From, { _DealInstrument, _DealTime, DealPrice, DealAmount } ) ->
	{ OpenTime, OpenPrice, _ClosePrice, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),

	NewReportData = case TotalAmount of
		0 ->
			%first deal here on instrument
			{ OpenTime, DealPrice, DealPrice, DealPrice, DealPrice, DealAmount };
		_Other ->
			RecTotalAmount = TotalAmount + DealAmount,
			RecClosePrice = DealPrice,
			RecMinPrice = min( MinPrice, DealPrice ),
			RecMaxPrice = max( MaxPrice, DealPrice ),
			{ OpenTime, OpenPrice, RecClosePrice, RecMinPrice, RecMaxPrice, RecTotalAmount }
	end,
	From ! { reply, self(), "Good deal" },

	set_report_data( NewReportData, State ).

send_report( DealerInstrument, State ) ->
	{ OpenTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),
	case TotalAmount of
		TotalAmount when TotalAmount > 0 ->
			bn_report:notify( { report, DealerInstrument, OpenTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } );
		_Other ->
			ignore
	end,
	exit( normal ).

loop( State, DealerInstrument ) ->
	DealerDateRange = get_datetime_setting( State ),
	{ _StartDatetime, EndDatetime, _DelaySeconds } = DealerDateRange,
	receive
		{ From, Deal } ->
			Expared = not datetime:datetime_earlier_than_datetime( datetime:now_datetime(), EndDatetime ),
			case Expared of
				true ->
					send_report( DealerInstrument, State );
				false ->
					ValidDealArgs = bn_common:validate_deal_args( [ DealerInstrument ], DealerDateRange, Deal ),

					NewState = case ValidDealArgs of
						true ->
							process_deal( State, From, Deal );
						{ error, ValidationErrorDescr } ->
							From ! { error, self(), ValidationErrorDescr },
							State
					end,

					loop( NewState, DealerInstrument )
			end;
		send_report ->
			send_report( DealerInstrument, State );
		Other ->
			io:fwrite( "Unhandled msg in dealer (should not happen): ~p~n", [Other] ),
			loop( State, DealerInstrument )
	end.

dealer( InstrumentName, DatetimeSettings ) ->
	%init here timer, state and etc

	{ StartDatetime, EndDatetime, _DelaySeconds } = DatetimeSettings,
	%TODO if datetime:now_datetime() > ExpirationDatetime ????
	Delay = datetime:datetime_difference_in_seconds( datetime:now_datetime(), EndDatetime ) * 1000,
	timer:send_after( Delay, send_report ),

	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	OpenDatetime = StartDatetime,
	InitialReportData = { OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	State = set_report_data( InitialReportData, dict:new() ),

	StateWithDatetime = dict:store( datetime_settings, DatetimeSettings, State ),

	loop( StateWithDatetime, InstrumentName ).

get_datetime_setting( State ) ->
	{ ok, DatetimeSetting } = dict:find( datetime_settings, State ),
	DatetimeSetting.

get_report_data( State ) ->
	{ ok, ReportData } = dict:find( report_data, State ),
	ReportData.

set_report_data( NewReportData, State ) ->
	dict:store( report_data, NewReportData, State ).
