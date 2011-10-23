-module(bn_common).

-export([validate_deal_args/6]).

valid_price( Price ) ->
	case is_number( Price ) of
		true when Price > 0 ->
			true;
		_Other ->
			false
	end.

valid_amount( Amount ) ->
	case is_integer( Amount ) of
		true when Amount > 0 ->
			true;
		_Other ->
			false
	end.

valid_deal_datetime( Datetime, DatesSettings ) ->
	ValidDatetime = datetime:valid_datetime( Datetime ),
	case ValidDatetime of
		true ->
			ValidDealDateRange = datetime:validDateTimeWithDateRangeAndDuration( Datetime, DatesSettings ),
			case ValidDealDateRange of
				true ->
					true;
				false ->
					{ error, "Invalid date: out of range deal dates" }
			end;
		false ->
			{ error, "Invalid date format" }
	end.

%returns true or { false, ValidateErrorDescription }
validate_deal_args( Instruments, DatesSettings, Instrument, Time, Price, Amount ) ->
	ValidDatetime   = valid_deal_datetime( Time, DatesSettings ),
	ValidInstrument = sets:is_element( Instrument, Instruments ),
	ValidPrice      = valid_price( Price ),
	ValidAmount     = valid_amount( Amount ),

	case { ValidDatetime, ValidInstrument, ValidPrice, ValidAmount } of
		{ true, true, true, true } ->
			true;
		{ { error, DatetimeValidationError }, _, _, _ } ->
			{ error, DatetimeValidationError };
		{ _, false, _, _ } ->
			{ error, "Invalid instrument name" };
		{ _, _, false, _ } ->
			{ error, "Invalid Price, It should be larger then zero" };
		{ _, _, _, false } ->
			{ error, "Invalid Amount, It should be larger then zero and integer" }
	end.
