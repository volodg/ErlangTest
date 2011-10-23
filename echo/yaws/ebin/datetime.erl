-module(datetime).

-export([addSecondToDatetime/2,datetimeWithinDatetimes/3]).

addSecondToDatetime(Seconds, Datetime) ->
	StartSeconds = calendar:datetime_to_gregorian_seconds( Datetime ),
	NewSeconds   = StartSeconds + Seconds,
	calendar:gregorian_seconds_to_datetime( NewSeconds ).

datetimeWithinDatetimes( Datetime, StartDatetime, EndDatetime ) ->
	%Seconds      = calendar:datetime_to_gregorian_seconds( Datetime ),
	StartSeconds = calendar:datetime_to_gregorian_seconds( StartDatetime ),
	EndSeconds   = calendar:datetime_to_gregorian_seconds( EndDatetime ),
	case calendar:datetime_to_gregorian_seconds( Datetime ) of
		% and Seconds < EndSeconds
		Seconds when ( StartSeconds =< Seconds ) and ( Seconds =< EndSeconds ) ->
			true;
		_Other ->
			false
	end.
