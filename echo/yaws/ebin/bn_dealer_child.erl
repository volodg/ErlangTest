-module(bn_dealer_child).

-behaviour(gen_server).

-include("bn_config.hrl").

%% API
-export([start_link/2]).

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
start_link( Parent, DatetimeSettings ) ->
	gen_server:start_link(?MODULE, [Parent, DatetimeSettings], []).

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
init( [ Parent, DatetimeSettings ] ) ->
	{ StartDatetime, EndDatetime, _DelaySeconds } = DatetimeSettings,

	Delay = datetime:datetime_difference_in_seconds( datetime:now_datetime(), EndDatetime ) * 1000,
	timer:send_after( Delay, send_report_part ),

	TotalAmount = 0,
	OpenPrice = 0,
	ClosePrice = 0,
	MinPrice = 0,
	MaxPrice = 0,
	OpenDatetime = StartDatetime,
	InitialReportData = { OpenDatetime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount },
	State = set_report_data( InitialReportData, dict:new() ),

	StateWithDatetime = dict:store( datetime_settings, DatetimeSettings, State ),
	InitialState = dict:store( parent, Parent, StateWithDatetime ),

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
% handle_call({deal,#deal{instrument="echo10"}}, _From, _State) ->
% 	exit(self(), kill);

handle_call({deal,Deal}, _From, State) ->
	DealerDateRange = get_datetime_setting( State ),
	{ _StartDatetime, EndDatetime, _DelaySeconds } = DealerDateRange,
	Expared = not datetime:datetime_earlier_than_datetime( datetime:now_datetime(), EndDatetime ),
	Response = case Expared of
		true ->
			send_report( State ),
			{stop, normal, State};
		false ->
			NewState = process_deal( State, Deal ),
			{reply, {ok, "Good deal"}, NewState}
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
handle_info(send_report_part, State) ->
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

get_dealer_parent( State ) ->
	{ ok, Parent } = dict:find( parent, State ),
	Parent.

get_datetime_setting( State ) ->
	{ ok, DatetimeSetting } = dict:find( datetime_settings, State ),
	DatetimeSetting.

get_report_data( State ) ->
	{ ok, ReportData } = dict:find( report_data, State ),
	ReportData.

set_report_data( ReportData, State ) ->
	dict:store( report_data, ReportData, State ).

process_deal( State, Deal ) ->
	%timer:sleep(10),%simulate difficult calculation
	{ OpenTime, OpenPrice, _ClosePrice, MinPrice, MaxPrice, TotalAmount } = get_report_data( State ),

	NowDatetime = datetime:now_datetime(),

	NewReportData = case TotalAmount of
		0 ->
			%first deal here on instrument
			OpenPriceWithDate = { Deal#deal.price, NowDatetime },
			{ OpenTime, OpenPriceWithDate, OpenPriceWithDate, Deal#deal.price, Deal#deal.price, Deal#deal.amount };
		_Other ->
			RecTotalAmount = TotalAmount + Deal#deal.amount,
			RecClosePrice = { Deal#deal.price, NowDatetime },
			RecMinPrice = min( MinPrice, Deal#deal.price ),
			RecMaxPrice = max( MaxPrice, Deal#deal.price ),
			{ OpenTime, OpenPrice, RecClosePrice, RecMinPrice, RecMaxPrice, RecTotalAmount }
	end,

	set_report_data( NewReportData, State ).

send_report( State ) ->
	Report = get_report_data( State ),
	Parent = get_dealer_parent( State ),
	Parent ! { self(), sub_report, Report },
	exit( normal ).
