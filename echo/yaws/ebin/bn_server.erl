%Модуль для запуска дилеров, валидирования аргументов,
%распределение запросов между дилерами по инструментам и датам
-module(bn_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
		stop/0,
        deal/1,
		deal/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("bn_config.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	case whereis(?SERVER) of
		undefined ->
			io:fwrite( "Server not runned~n" );
		Pid ->
			exit( Pid, normal )
	end.

%%--------------------------------------------------------------------
%% Function: notify(Msg) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
deal( Deal ) ->
	%validate arguments here, in client thread !!!
	Response = gen_server:call( { ?SERVER, ?SRV_NODE }, {get_dealer, Deal}),
	case Response of
		{ error, ValidationErrorDescr } ->
			{ error, ValidationErrorDescr };
		{ dealer_pid, DealerPid } ->
			bn_dealer:deal(DealerPid, Deal)
	end.

deal( Instrument, Datetime, Price, Amount ) ->
	Deal = #deal{ instrument=Instrument,
	 				datetime=Datetime,
					price   =Price,
					amount  =Amount },
	deal( Deal ).

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
init([]) ->
	process_flag(trap_exit, true),

	io:fwrite( "Init with instruments: ~p~n", [?INSTRUMENTS] ),

	State = dict:store( dealer_info_by_instrument, dict:new(), dict:new() ),

	StartDatetime = start_datetime(),
	{ EndDatetime, Duration } = end_datetime( StartDatetime ),
	NewState = dict:store( dete_settings, { StartDatetime, EndDatetime, Duration }, State ),

	{ ok, NewState }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_dealer, Deal}, _From, State) ->
	process_get_dealer( State, Deal ).

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT',_Pid,normall}, State) ->
	%ignore this 'EXIT'
	{noreply, State};

handle_info({'EXIT',Pid,_Reason}, State) ->
	NewState = remove_dealer_pid( Pid, State ),
	{noreply, NewState};

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

start_datetime() ->
	%START_DATETIME macros should be used
	Datetime = datetime:now_datetime(),
	io:fwrite( "StartDate: ~p~n", [ Datetime ] ),
	Datetime.

end_datetime( StartDatetime ) ->
	DuratonInSeconds = ?REPORT_DURATION_SEC,
	io:fwrite( "DuratonInSeconds: ~p~n", [DuratonInSeconds] ),
	{EndDate, EndTime} = datetime:add_second_to_datetime( DuratonInSeconds * ?REPORT_DURATION_NUM, StartDatetime ),
	io:fwrite( "EndDate: {~p,~p}~n", [EndDate, EndTime] ),
	{ {EndDate, EndTime}, DuratonInSeconds }.

get_dates_settings( State ) ->
	{ ok, DateSettings } = dict:find( dete_settings, State ),
	DateSettings.

%Запуск дилера для инструмента и временного интервала торгов
run_new_dealer_for_instrument( State, Instrument ) ->
	DateSettings = get_dates_settings( State ),
	{ _StartDatetime, _EndDatetime, Duration } = DateSettings,
	EndDealerDatetime = datetime:nearest_expiration_datetime( DateSettings ),
	StartDealerDatetime = datetime:add_second_to_datetime( -?REPORT_DURATION_SEC, EndDealerDatetime ),
	DealerDateRange = { StartDealerDatetime, EndDealerDatetime, Duration },
	{ok,DealerPid} = bn_dealer:start_link( Instrument, DealerDateRange ),
	NewState = set_dealer_info( State, Instrument, { DealerPid, EndDealerDatetime } ),
	{ NewState, DealerPid }.

%validate arguments before calling this method
%Ищет дилера по инструменту
%Если не находит - создает нового
%Иначе возвращает старого если его временной промежуток торгов еще не закончился
process_get_dealer_for_instrument( State, Deal ) ->
	FindResult = find_dealer_info( State, Deal#deal.instrument ),
	{ NewState, InstrumentDealerPid } = case FindResult of
		{ ok, { DealerPid, EndDealerDatetime } } ->
			ExpirationDateExpared = not datetime:datetime_earlier_than_datetime( datetime:now_datetime(), EndDealerDatetime ),
		    case ExpirationDateExpared of
				true ->
					run_new_dealer_for_instrument( State, Deal#deal.instrument );
				false ->
					{ State, DealerPid }
			end;
		error ->
			run_new_dealer_for_instrument( State, Deal#deal.instrument )
	end,
	{ reply, { dealer_pid, InstrumentDealerPid }, NewState }.

process_get_dealer( State, Deal ) ->
	ValidDealArgs = bn_common:validate_deal_args( ?INSTRUMENTS, get_dates_settings( State ), Deal ),

	case ValidDealArgs of
		true ->
			process_get_dealer_for_instrument( State, Deal );
		{ error, ValidationErrorDescr } ->
			{reply, { error, ValidationErrorDescr }, State}
	end.

remove_dealer_pid( Pid, State ) ->
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	NewDealerPidAndExpDateByInstrument = dict:filter(fun(_Key, { DealerPid, _ExpDate }) ->
		 case DealerPid of
			Pid ->
				false;
			_Other ->
				true
		 end
		end, DealerPidAndExpDateByInstrument),
	dict:store( dealer_info_by_instrument, NewDealerPidAndExpDateByInstrument, State ).

%returns { ok, { DealerPid, ExpirationDate } } or error
find_dealer_info( State, Instrument ) ->
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	dict:find( Instrument, DealerPidAndExpDateByInstrument ).

%returns NewState
set_dealer_info( State, Instrument, DealerInfo ) ->
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	NewDealerPidAndExpDateByInstrument = dict:store( Instrument, DealerInfo, DealerPidAndExpDateByInstrument ),
	dict:store( dealer_info_by_instrument, NewDealerPidAndExpDateByInstrument, State ).
