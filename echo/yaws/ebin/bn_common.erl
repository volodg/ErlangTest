-module(bn_common).

-export([validate_deal_args/3,priority_receive/2,random_deal/0]).

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
			{ error, "Invalid date format" }
	end.

%returns true or { false, ValidateErrorDescription }
validate_deal_args( Instruments, DatesSettings, Deal ) ->
	{ Instrument, Time, Price, Amount } = Deal,
	ValidDatetime   = valid_deal_datetime( Time, DatesSettings ),
	ValidInstrument = sets:is_element( Instrument, sets:from_list( Instruments ) ),
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

%TODO remove
priority_receive( Term, Otherwise ) ->
	receive
		Term ->
			Term
	    after 0 ->
	    	Otherwise()
	end.

current_datetime() ->
	%datetime:add_second_to_datetime( 60, { date(), time() } ).
	{ date(), time() }.

random_instrument() ->
	%"echo23".
	lists:append( "echo", integer_to_list(random:uniform(10)) ).

random_price() ->
	erlang:round( ( random:uniform() + 1 ) * 100 ) / 100.

random_amount() ->
	random:uniform(1000).

random_deal() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),

	DateTime = current_datetime(),
	Instrument = random_instrument(),
	Price = random_price(),
	Amount = random_amount(),

	{ Instrument, DateTime, Price, Amount }.
