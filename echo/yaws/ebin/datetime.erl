-module(datetime).

-export([add_second_to_datetime/2
,datetime_within_datetimes/3
,datetime_earlier_than_datetime/2
,nearest_datetime_less_than_now/2
,valid_datetime_with_dates_settings/2
,valid_datetime/1
,nearest_expiration_datetime/1
,datetime_difference_in_seconds/2]).

add_second_to_datetime(Seconds, Datetime) ->
	StartSeconds = calendar:datetime_to_gregorian_seconds( Datetime ),
	NewSeconds   = StartSeconds + Seconds,
	calendar:gregorian_seconds_to_datetime( NewSeconds ).

datetime_earlier_than_datetime( Datetime, ThanDatetime ) ->
	ThanSeconds = calendar:datetime_to_gregorian_seconds( ThanDatetime ),
	case calendar:datetime_to_gregorian_seconds( Datetime ) of
		Seconds when ( Seconds < ThanSeconds ) ->
			true;
		_Other ->
			false
	end.

datetime_within_datetimes( Datetime, StartDatetime, EndDatetime ) ->
	StartSeconds = calendar:datetime_to_gregorian_seconds( StartDatetime ),
	EndSeconds   = calendar:datetime_to_gregorian_seconds( EndDatetime ),
	case calendar:datetime_to_gregorian_seconds( Datetime ) of
		Seconds when ( StartSeconds =< Seconds ) and ( Seconds =< EndSeconds ) ->
			true;
		_Other ->
			false
	end.

datetime_difference_in_seconds( FromDatetime, ToDatetime ) ->
	FromDatetimeSeconds = calendar:datetime_to_gregorian_seconds( FromDatetime ),
	ToDatetimeSeconds = calendar:datetime_to_gregorian_seconds( ToDatetime ),
	ToDatetimeSeconds - FromDatetimeSeconds.

%TODO remove recursion
nearest_datetime_less_than_now( StartDateTime, Duration ) ->
	NowDateTime = { date(), time() },
	NextCurrentDateTime = add_second_to_datetime( Duration, StartDateTime ),
	LessThen = datetime_earlier_than_datetime( NowDateTime, NextCurrentDateTime ),
	case LessThen of
		false ->
			nearest_datetime_less_than_now( NextCurrentDateTime, Duration );
		true ->
			StartDateTime
	end.

nearest_expiration_datetime( DatesSettings ) ->
	{ StartDatetime, _EndDatetime, Duration } = DatesSettings,
	CurrentStartTime = nearest_datetime_less_than_now( StartDatetime, Duration ),
	add_second_to_datetime( Duration, CurrentStartTime ).

valid_datetime_with_dates_settings( Datetime, DatesSettings ) ->
	{ StartDatetime, EndDatetime, Duration } = DatesSettings,
	NowDatetime = { date(), time() },
	ValidNowDatetime = datetime_within_datetimes( NowDatetime, StartDatetime, EndDatetime ),
	CurrentStartTime = nearest_datetime_less_than_now( StartDatetime, Duration ),
	ValidDatetime = datetime_within_datetimes( Datetime, CurrentStartTime, NowDatetime ),
	case { ValidNowDatetime, ValidDatetime } of
		{ true, true } ->
			true;
		{ _, false } ->
			false;
		{ false, _ } ->
			false
	end.

valid_time({H,M,S}) -> valid_time(H,M,S).

valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

valid_datetime( Datetime ) ->
	case Datetime of
		{ {Year,Month,Day}, {Hour,Minute,Seconds} } ->
			ValidDate = calendar:valid_date( {Year,Month,Day} ),
			ValidTime = valid_time( {Hour,Minute,Seconds} ),
			case { ValidDate, ValidTime } of
				{ true, true } ->
					true;
				_Other ->
					false
			end;
		_Other ->
			false
	end.
