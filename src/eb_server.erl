%% @author henry
%% @doc @todo Add description to eb_server.


-module(eb_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("eunit/include/eunit.hrl").
-export([server_test_/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, create_account/2, deposit/2, withdraw/2, 
	authorize/2, delete_account/1]).
-define(SERVER,?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%% ServerName       = {local, Name} or {global, Name}. Name server to be registered,
%%                    if you want to ommit, use start_link/3
%% CallbackModule   = the name of the module in which the specific callback
%%                    functions are placed
%% Arguments        = Erlang term passed to the init/1 callback function.
%% Options          = Memory management flags, fullsweep_after and heapsize
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: create_account(Name) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
create_account(Name, PIN) ->
  gen_server:cast(?SERVER, {create, Name, PIN}).

%%--------------------------------------------------------------------
%% Function: deposit(Name, Amount) -> {ok, Balance} | {error, Reason}
%% Description: Deposits Amount into Name's account. Returns the
%% balance if successful, otherwise returns an error and reason.
%%--------------------------------------------------------------------
deposit(Name, Amount) ->
  gen_server:call(?SERVER, {deposit, Name, Amount}).

%%--------------------------------------------------------------------
%% Function: withdraw(Name, Amount) -> {ok, Balance} | {error, Reason}
%% Description: Withdraws Amount from Name's account.
%%--------------------------------------------------------------------
withdraw(Name, Amount) ->
  gen_server:call(?SERVER, {withdraw, Name, Amount}).

%%--------------------------------------------------------------------
%% Function: delete_account(Name) -> ok
%% Description: Deletes the account with the name Name.
%%--------------------------------------------------------------------
delete_account(Name) ->
  gen_server:cast(?SERVER, {destroy, Name}).

%%--------------------------------------------------------------------
%% Function: authorize(Name, Pin) -> ok | {error, Reason}
%% Description: Authorizes the account Name with PIN
%%--------------------------------------------------------------------
authorize(Name, PIN) ->
  gen_server:call(?SERVER, {authorize, Name, PIN}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason:: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, dict:new()}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================


handle_call({authorize, Name, PIN}, _From, State) ->
	case dict:find(Name, State) of
		{ok, {PIN, _Value}} ->
			{reply, ok, State};
		{ok, {_OtherPIN, _Value}} ->
			{reply, {error, invalid_pin}, State};
		error ->
			{reply, {error, account_does_not_exist}, State}
	end;

handle_call({deposit, Name, Amount}, _From, State) ->
  case dict:find(Name, State) of
    {ok, {PIN, Value}} ->
      NewBalance = Value + Amount,
      Response = {ok, NewBalance},
      NewState = dict:store(Name, {PIN, NewBalance}, State),
      {reply, Response, NewState};
    error ->
      {reply, {error, account_does_not_exist}, State}
  end;

handle_call({withdraw, Name, Amount}, _From, State) ->
  case dict:find(Name, State) of
	{ok, {PIN, Value}} when Value < Amount ->
      {reply, {error, not_enough_funds}, State};
    {ok, {PIN, Value}} ->
      NewBalance = Value - Amount,
      NewState = dict:store(Name, {PIN, NewBalance}, State),
      {reply, {ok, {PIN, NewBalance}}, NewState};
    error ->
      {reply, {error, account_does_not_exist}, State}

  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.



%% handle_cast/2sdf
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({create, Name, PIN}, State) ->
  {noreply, dict:store(Name, {PIN,0}, State)};

handle_cast({destroy, Name}, State) ->
  {noreply, dict:erase(Name, State)};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% EUnit tests
%% ====================================================================

server_test_() ->
	[?_assert(eb_server:start_link() =:= ok)].

