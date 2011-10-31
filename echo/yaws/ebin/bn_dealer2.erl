-module(bn_dealer).

-behaviour(gen_server).

-include("bn_config.hrl").

%% API
-export([start_link/2,deal/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link( InstrumentName, DatetimeSettings ) ->
	gen_server:start_link(?MODULE, [InstrumentName, DatetimeSettings], []).

deal(DealerPid, Deal) ->
	case catch( gen_server:call( DealerPid, {deal, Deal}) ) of
		{'EXIT', _Msg} ->
			{ error, "Dealer already unavailable" };
		Other ->
			Other
	end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init( [ InstrumentName, DatetimeSettings ] ) ->
	{ StartDatetime, EndDatetime, _DelaySeconds } = DatetimeSettings,

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
	InitialState = dict:store( dealer_instrument, InstrumentName, StateWithDatetime ),

	{ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({deal,Deal}, _From, State) ->
	DealerDateRange = get_datetime_setting( State ),
	{ _StartDatetime, EndDatetime, _DelaySeconds } = DealerDateRange,
	Expared = not datetime:datetime_earlier_than_datetime( datetime:now_datetime(), EndDatetime ),
	Response = case Expared of
		true ->
			send_report( State ),
			{stop, normal, State};
		false ->
			ValidDealArgs = bn_common:validate_deal_args( [ get_dealer_instrument( State ) ], DealerDateRange, Deal ),

			case ValidDealArgs of
				true ->
					NewState = process_deal( State, Deal ),
					{reply, {ok, "Good deal"}, NewState};
				{ error, ValidationErrorDescr } ->
					{reply, {error, ValidationErrorDescr}, State}
			end
	end,
	Response;

handle_call(_Request, _From, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(send_report, State) ->
	send_report( State ),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_dealer_instrument( State ) ->
	{ ok, DealerInstrument } = dict:find( dealer_instrument, State ),
	DealerInstrument.

get_datetime_setting( State ) ->
	{ ok, DatetimeSetting } = dict:find( datetime_settings, State ),
	DatetimeSetting.

get_report_data( State ) ->
	{ ok, ReportData } = dict:find( report_data, State ),
	ReportData.

set_report_data( ReportData, State ) ->
	dict:store( report_data, ReportData, State ).

process_deal( State, Deal ) ->
	{ OpenTime, OpenPrice, _ClosePrice, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),

	NewReportData = case TotalAmount of
		0 ->
			%first deal here on instrument
			{ OpenTime, Deal#deal.price, Deal#deal.price, Deal#deal.price, Deal#deal.price, Deal#deal.amount };
		_Other ->
			RecTotalAmount = TotalAmount + Deal#deal.amount,
			RecClosePrice = Deal#deal.price,
			RecMinPrice = min( MinPrice, Deal#deal.price ),
			RecMaxPrice = max( MaxPrice, Deal#deal.price ),
			{ OpenTime, OpenPrice, RecClosePrice, RecMinPrice, RecMaxPrice, RecTotalAmount }
	end,

	set_report_data( NewReportData, State ).

send_report( State ) ->
	{ OpenTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),
	case TotalAmount of
		TotalAmount when TotalAmount > 0 ->
			Report = #report{instrument  =get_dealer_instrument( State ),
							 open_time   =OpenTime,
							 open_price  =OpenPrice,
							 close_price =ClosePrice,
							 min_price   =MinPrice,
							 max_price   =MaxPrice,
							 total_amount=TotalAmount},
			bn_report:notify( Report );
		_Other ->
			ignore
	end.
