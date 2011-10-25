-module(bn_server).

-behaviour(gen_server).

%% API
-export([start/0,
        deal/1]).

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
start() ->
	case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
		{ok,_Pid} ->
			ok;
		{error,{already_started,Pid}} ->
			exit(Pid, kill),
			{error,{already_started,Pid}};
		Other ->
			Other
	end.

%%--------------------------------------------------------------------
%% Function: notify(Msg) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
deal( Deal ) ->
	Response = gen_server:call( { ?SERVER, ?SRV_NODE }, {get_dealer, Deal}),
	case Response of
		{ error, ValidationErrorDescr } ->
			{ error, ValidationErrorDescr };
		{ dealer_pid, DealerPid } ->
			DealerPid ! { self(), Deal },
			%TODO change this
			receive
				{ reply, Msg } ->
					{ ok, Msg };
				{ error, ErrorDescr } ->
					{ error, ErrorDescr };
				Other ->
					io:fwrite( "Other!!!: ~p~n", [Other] )
			after 500 ->
				timeout
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
init([]) ->
	io:fwrite( "Init with instruments: ~p~n", [?INSTRUMENTS] ),

	bn_report:start_link(),

	State = dict:store( dealer_info_by_instrument, dict:new(), dict:new() ),

	StartDateTime = start_datetime(),
	{ EndDatetime, Duration } = end_datetime( StartDateTime ),
	NewState = dict:store( dete_settings, { StartDateTime, EndDatetime, Duration }, State ),

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
	%{reply, Reply, State}.

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
	StartDate = date(),
	StartTime = time(),
	io:fwrite( "StartDate: {~p,~p}~n", [ StartDate, StartTime ] ),
	{ date(), time() }.

end_datetime( StartDateTime ) ->
	DuratonInSeconds = ?REPORT_DURATION_SEC,
	io:fwrite( "DuratonInSeconds: ~p~n", [DuratonInSeconds] ),
	%TODO change 10000
	{EndDate, EndTime} = datetime:add_second_to_datetime( DuratonInSeconds * 10000, StartDateTime ),
	io:fwrite( "EndDate: {~p,~p}~n", [EndDate, EndTime] ),
	{ {EndDate, EndTime}, DuratonInSeconds }.

get_dates_settings( State ) ->
	{ ok, DateSettings } = dict:find( dete_settings, State ),
	DateSettings.

run_new_dealer_for_instrument( State, Instrument ) ->
	ExpirationDatetime = datetime:nearest_expiration_datetime( get_dates_settings( State ) ),
	DealerPid = spawn( bn_dealer, dealer, [Instrument, ExpirationDatetime] ),
	NewState = set_dealer_info( State, Instrument, { DealerPid, ExpirationDatetime } ),
	{ NewState, DealerPid }.

%validate arguments before calling this method
process_get_dealer_for_instrument( State, { Instrument, _Time, _Price, _Amount } ) ->
	{ NewState, InstrumentDealerPid } = case find_dealer_info( State, Instrument ) of
		{ ok, { DealerPid, ExpirationDate } } ->
			NearestNextStartDate = datetime:nearest_expiration_datetime( get_dates_settings( State ) ),
			ExpirationDateExpared = datetime:datetime_earlier_than_datetime( ExpirationDate, NearestNextStartDate ),
		    case ExpirationDateExpared of
				true ->
					run_new_dealer_for_instrument( State, Instrument );
				false ->
					{ State, DealerPid }
			end;
		error ->
			run_new_dealer_for_instrument( State, Instrument )
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

%returns { ok, { DealerPid, ExpirationDate } } or error
find_dealer_info( State, Instrument ) ->
	%io:fwrite( "Print State: ~p~n", [State] ),
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	dict:find( Instrument, DealerPidAndExpDateByInstrument ).

%returns NewState
set_dealer_info( State, Instrument, DealerInfo ) ->
	{ ok, DealerPidAndExpDateByInstrument } = dict:find( dealer_info_by_instrument, State ),
	NewDealerPidAndExpDateByInstrument = dict:store( Instrument, DealerInfo, DealerPidAndExpDateByInstrument ),
	dict:store( dealer_info_by_instrument, NewDealerPidAndExpDateByInstrument, State ).
