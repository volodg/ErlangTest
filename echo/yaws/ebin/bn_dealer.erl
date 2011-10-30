-module(bn_dealer).

-behaviour(gen_server).

-include("bn_config.hrl").

%% API
-export([start_link/2,deal/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
	case catch( gen_server:call( DealerPid, get_dealer) ) of
		{normal, _Msg} ->
			{ error, "Dealer already unavailable" };
		SubDealerPid ->
			case catch( gen_server:call( SubDealerPid, {deal, Deal}) ) of
				{normal, _Msg} ->
					{ error, "Dealer already unavailable" };
				Other ->
					Other
			end
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
	State               = set_reporting( false, dict:new() ),
	StateWithSubDealers = dict:store( sub_dealers, [], State ),
	StateWithDatetime   = dict:store( datetime_settings, DatetimeSettings, StateWithSubDealers ),
	StateWithInstrument = dict:store( dealer_instrument, InstrumentName, StateWithDatetime ),

	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	OpenDatetime = 0,
	InitialReportData = { OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	InitialState = set_report_data( InitialReportData, StateWithInstrument ),

	{ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%ETODO process exit of child
%remove child Pid in this case

%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_dealer, _From, State) ->
	case get_reporting( State ) of
		true ->
			{reply, {error, "Out of date deal"}, State};
		false ->
			{ NewState, SubDealerPid } = get_dealer_child( State ),
			{reply, SubDealerPid, NewState}
	end;

handle_call(_Request, _From, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info( { SubDealerPid, sub_report, Report }, State) ->
	NewState = set_reporting( true, State ),
	collect_report( SubDealerPid, Report, NewState );

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

get_existing_rec_for_key( Key, State ) ->
	{ ok, Result } = dict:find( Key, State ),
	Result.

get_reporting( State ) ->
	get_existing_rec_for_key( reporting, State ).

set_reporting( Flag, State ) ->
	dict:store( reporting, Flag, State ).

get_sub_dealers( State ) ->
	get_existing_rec_for_key( sub_dealers, State ).

set_sub_dealers( SubDealers, State ) ->
	dict:store( sub_dealers, SubDealers, State ).

get_dealer_instrument( State ) ->
	get_existing_rec_for_key( dealer_instrument, State ).

get_datetime_setting( State ) ->
	get_existing_rec_for_key( datetime_settings, State ).

get_report_data( State ) ->
	get_existing_rec_for_key( report_data, State ).

set_report_data( ReportData, State ) ->
	dict:store( report_data, ReportData, State ).

create_sub_dealer( State ) ->
	DealerDateRange = get_datetime_setting( State ),
	{ok,SubDealerPid} = bn_dealer_child:start_link( self(), DealerDateRange ),
	SubDealerPid.

%ETODO if child dealer is free it should pass self to begin of list
get_dealer_child( State ) ->
	SubDealers = get_sub_dealers( State ),
	case length( SubDealers ) of
		Count when Count >= ?DEALERS_PER_INSTRUMENT ->
			[ Pid | OtherPids ] = SubDealers,
			NewState = set_sub_dealers( lists:append( OtherPids, [Pid] ), State ),
			{ NewState, Pid };
		_Other ->
			Pid = create_sub_dealer( State ),
			NewState = set_sub_dealers( lists:append( SubDealers, [Pid] ), State ),
			{ NewState, Pid }
	end.

send_report( State ) ->
	{ OpenDatetime, OpenPriceDatetime, ClosePriceDatetime, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),

	case TotalAmount of
		0 ->
			true;
		_Other ->
			{ OpenPrice, _OpenPriceDateTime } = OpenPriceDatetime,
			{ ClosePrice, _ClosePriceDateTime } = ClosePriceDatetime,
			Report = #report{instrument =get_dealer_instrument( State ),
							open_time   =OpenDatetime,
							open_price  =OpenPrice,
							close_price =ClosePrice,
							min_price   =MinPrice,
							max_price   =MaxPrice,
							total_amount=TotalAmount},
			bn_report:notify( Report )
	end.

process_sub_dealer_report( Report, State ) ->
	{ OpenDatetime, OpenPriceDatetime, ClosePriceDatetime, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),
	case TotalAmount of
		0 ->
			set_report_data( Report, State );
		_TotalAmount ->
			{ DealOpenDatetime, DealOpenPriceDatetime, DealClosePriceDatetime, DealMinPrice, DealMaxPrice, DealTotalAmount } = Report,
			case DealTotalAmount of
				0 ->
					State;
				_DealTotalAmount ->
					NewOpenDatetime = datetime:less_datetime( DealOpenDatetime, OpenDatetime ),

					{ _OpenPrice, OpenPriceDateTime } = OpenPriceDatetime,
					{ _DealOpenPrice, DealOpenPriceDateTime } = DealOpenPriceDatetime,
					NewOpenPrice = case datetime:datetime_earlier_than_datetime( OpenPriceDateTime, DealOpenPriceDateTime ) of
						true ->
							OpenPriceDatetime;
						false ->
							DealOpenPriceDatetime
					end,

					{ _ClosePrice, ClosePriceDateTime } = ClosePriceDatetime,
					{ _DealClosePrice, DealClosePriceDateTime } = DealClosePriceDatetime,
					NewClosePrice = case datetime:datetime_earlier_than_datetime( ClosePriceDateTime, DealClosePriceDateTime ) of
						true ->
							DealClosePriceDatetime;
						false ->
							ClosePriceDatetime
					end,

					NewMinPrice = min( MinPrice, DealMinPrice ),
					NewMaxPrice = max( MaxPrice, DealMaxPrice ),
					NewTotalAmount = TotalAmount + DealTotalAmount,
					NewReport = { NewOpenDatetime, NewOpenPrice, NewClosePrice, NewMinPrice, NewMaxPrice, NewTotalAmount },
					set_report_data( NewReport, State )
			end
	end.

%ETODO process exit of child
collect_report( SubDealerPid, Report, State ) ->
	NewState = process_sub_dealer_report( Report, State ),
	SubDealers = get_sub_dealers( NewState ),
	NewSubDealers = lists:delete( SubDealerPid, SubDealers ),
	NewState2 = set_sub_dealers( NewSubDealers, NewState ),
	case NewSubDealers of
		[] ->
			send_report( NewState2 ),
			{stop, normal, NewState2};
		_Other2 ->
			{noreply, NewState2}
	end.
