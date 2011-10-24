-module(datetime).

-export([addSecondToDatetime/2
,datetimeWithinDatetimes/3
,datetimeEarlierThanDatetime/2
,nearestDatetimeLessThanNow/2
,validDateTimeWithDateRangeAndDuration/2
,valid_datetime/1
,nearestExpirationDatetime/1
,datetime_difference_in_seconds/2]).

addSecondToDatetime(Seconds, Datetime) ->
	StartSeconds = calendar:datetime_to_gregorian_seconds( Datetime ),
	NewSeconds   = StartSeconds + Seconds,
	calendar:gregorian_seconds_to_datetime( NewSeconds ).

datetimeEarlierThanDatetime( Datetime, ThanDatetime ) ->
	ThanSeconds = calendar:datetime_to_gregorian_seconds( ThanDatetime ),
	case calendar:datetime_to_gregorian_seconds( Datetime ) of
		Seconds when ( Seconds < ThanSeconds ) ->
			true;
		_Other ->
			false
	end.

datetimeWithinDatetimes( Datetime, StartDatetime, EndDatetime ) ->
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
nearestDatetimeLessThanNow( StartDateTime, Duration ) ->
	NowDateTime = { date(), time() },
	NextCurrentDateTime = addSecondToDatetime( Duration, StartDateTime ),
	LessThen = datetimeEarlierThanDatetime( NowDateTime, NextCurrentDateTime ),
	case LessThen of
		false ->
			nearestDatetimeLessThanNow( NextCurrentDateTime, Duration );
		true ->
			StartDateTime
	end.

nearestExpirationDatetime( DatesSettings ) ->
	{ StartDatetime, _EndDatetime, Duration } = DatesSettings,
	CurrentStartTime = nearestDatetimeLessThanNow( StartDatetime, Duration ),
	addSecondToDatetime( Duration, CurrentStartTime ).

validDateTimeWithDateRangeAndDuration( Datetime, DatesSettings ) ->
	{ StartDatetime, EndDatetime, Duration } = DatesSettings,
	NowDatetime = { date(), time() },
	ValidNowDatetime = datetimeWithinDatetimes( NowDatetime, StartDatetime, EndDatetime ),
	CurrentStartTime = nearestDatetimeLessThanNow( StartDatetime, Duration ),
	ValidDatetime = datetimeWithinDatetimes( Datetime, CurrentStartTime, NowDatetime ),
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
		{ Date, Time } ->
			ValidDate = calendar:valid_date( Date ),
			ValidTime = valid_time( Time ),
			case { ValidDate, ValidTime } of
				{ true, true } ->
					true;
				_Other ->
					false
			end;
		_Other ->
			false
	end.
