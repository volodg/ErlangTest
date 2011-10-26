-module(bn_report).

-behaviour(gen_server).

-include("bn_config.hrl").

%% API
-export([start_link/0,
        notify/1,
		subscribe/1,
		unsubscribe/1]).

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
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: notify(Msg) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
notify(Msg) ->
	gen_server:cast( { ?SERVER, ?SRV_NODE }, {notify_all, Msg}).

%TODO think again
subscribe(_Pid) ->
	gen_server:cast( { ?SERVER, ?SRV_NODE }, {subscribe, _Pid}).
	%gen_server:call( { ?SERVER, ?SRV_NODE }, subscribe ).

%TODO remove Pid argument
unsubscribe(Pid) ->
	gen_server:cast( { ?SERVER, ?SRV_NODE }, {unsubscribe, Pid}).

%TODO do notification via "gen_server:call" !!!

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
	%TODO send live packages
	{ok, []}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast( { notify_all, Msg }, State ) ->
	io:fwrite( "notify_all with report: ~p~n", [ State ] ),
	lists:foreach(fun(H) -> H ! Msg end, State),
	{noreply, State};

handle_cast({subscribe, Pid}, State) ->
	NewState = lists:append(State, [Pid]),
	{noreply, NewState};

handle_cast({unsubscribe, Pid}, State) ->
	NewState = lists:delete(Pid, State),
	{noreply, NewState}.

%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(subscribe, From, State) ->
	NewState = lists:append(State, [From]),
	{reply, ok, NewState};

handle_call(_Request, _From, State) ->
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
terminate(_Reason, State) ->
	lists:foreach(fun(H) -> H ! finish end, State),
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