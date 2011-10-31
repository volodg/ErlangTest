-module(bn_common).

-export([validate_deal_args/3,random_deal/0,random_instrument/0]).

-include("bn_config.hrl").

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
			ValidDealDateRange = datetime:valid_datetime_with_dates_settings( Datetime, DatesSettings ),
			case ValidDealDateRange of
				true ->
					true;
				false ->
					{ error, "Invalid date: out of range deal dates" }
			end;
		false ->
			{ error, "Invalid datetime format" }
	end.

%returns true or { false, ValidateErrorDescription }
validate_deal_args( Instruments, DatesSettings, Deal ) ->
	ValidDatetime   = valid_deal_datetime( Deal#deal.datetime, DatesSettings ),
	ValidInstrument = sets:is_element( Deal#deal.instrument, sets:from_list( Instruments ) ),
	ValidPrice      = valid_price( Deal#deal.price ),
	ValidAmount     = valid_amount( Deal#deal.amount ),

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

current_datetime() ->
	datetime:now_datetime().

random_instrument() ->
	Length  = length( ?INSTRUMENTS ),
	[ Instrument | [] ] = lists:sublist(?INSTRUMENTS, random:uniform(Length), 1),
	Instrument.

random_price() ->
	erlang:round( ( random:uniform() + 1 ) * 100 ) / 100.

random_amount() ->
	random:uniform(1000).

random_deal() ->
	#deal{instrument=random_instrument(),
	 		datetime=current_datetime(),
			price   =random_price(),
			amount  =random_amount()}.
