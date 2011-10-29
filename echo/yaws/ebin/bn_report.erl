-module(bn_report).

-behaviour(gen_server).

-include("bn_config.hrl").

%% API
-export([start_link/0,
		stop/0,
        notify/1,
		subscribe/0,
		unsubscribe/0]).

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

stop() ->
	case whereis(?SERVER) of
		undefined ->
			io:fwrite( "Report not runned~n" );
		Pid ->
			exit( Pid, normal )
	end.

%%--------------------------------------------------------------------
%% Function: notify(Msg) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
notify(Msg) ->
	gen_server:cast( { ?SERVER, ?SRV_NODE }, {notify_all, Msg}).

subscribe() ->
	gen_server:call( { ?SERVER, ?SRV_NODE }, subscribe).

unsubscribe() ->
	gen_server:call( { ?SERVER, ?SRV_NODE }, unsubscribe).

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
	timer:send_after( 5000, send_live_pkg ),
	{ok, []}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast( { notify_all, Msg }, State ) ->
	lists:foreach(fun(H) -> H ! { self(), Msg } end, State),
	{noreply, State}.

%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(subscribe, From, State) ->
	{Pid,_Tag} = From,
	NewState = subscribe_pid(State, Pid),
	{reply, self(), NewState};

handle_call(unsubscribe, From, State) ->
	{Pid,_Tag} = From,
	NewState = lists:delete(Pid, State),
	{reply, ok, NewState};

handle_call(_Request, _From, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(send_live_pkg, State) ->
	timer:send_after( 5000, send_live_pkg ),
	lists:foreach(fun(H) -> H ! { self(), live_pkg } end, State),
	{noreply, State};

handle_info({'EXIT',Pid,_Reason}, State) ->
	NewState = lists:delete(Pid, State),
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
terminate(_Reason, State) ->
	lists:foreach(fun(H) -> H ! { self(), finish } end, State),
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

subscribe_pid(State, Pid) ->
	link( Pid ),
	lists:append(State, [Pid]).
