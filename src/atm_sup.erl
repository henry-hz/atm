
-module(atm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% CHILD SPECIFICATION     {ID, {Module, Function, [Args]}, Restart, Shutdown, Type, [Modules] } 
%% ID                    : term that the supervisor uses to identify the specification internally.
%% {Module, Fun, [Args]} : module, function and arguments to start the process
%% Restart  = permanent  : long-lived, so always be restarted if it terminates for any reason.
%%            temporary  : should never be restarted.
%%            transient  : restarted only if they terminate abnormally but not 
%%                         upon normal termination.
%% Shutdown = [milli]    : soft shutdown and the process has that many milliseconds to shut itself 
%%                         down before it’s unconditionally killed
%%            brutal_kill: the process will always be terminated immediately upon shutdown.
%%            infinity   : used mainly when the child itself is a supervisor and should be given 
%%                         all the time it needs
%% Type     = supervisor : expects the definition of which child processes to supervise 
%%                         to be specified in a callback module
%%            worker     : normally implemented using one of the gen_event, gen_fsm, 
%%                         or gen_server behaviours.
%% Modules               : this information is used only during hot code upgrades and indicates to 
%%                         the system in what order modules should be upgraded. Generally, you only 
%%                         need to list the main module for the child processes.




init([]) -> 
	Proc = {eb_server, {eb_server, start_link, []}, 
		      permanent, 5000, supervisor, [eb_server]},
 	Proc2= {eb_atm, {eb_atm, start_link, []},
 			  permanent, 5000, supervisor, [eb_atm]},
Children = [Proc, Proc2],

%% RESTART STRATEGY        {Strategy, Maximum, Timeframe}
%% one_for_one           : if one child process terminates and should be restarted, 
%%                         only that child process is affected.
%% one_for_all           : if one child process terminates and should be restarted, all 
%%                         other child processes are terminated and then all child 
%%                         processes are restarted.
%% rest_for_one          : if one child process terminates and should be restarted, 
%%                         the 'rest' of the child processes.
%% simple_one_for_one    : a simplified one_for_one supervisor, where all child processes are 
%%                         dynamically added instances of the same process type, i.e. running the same code
%% Maximum               : maximum number of restarts.
%% Timeframe             : restart frequency in seconds. e.g. Max=10 and Timeframe=30, you allow at 
%%                         most 10 restarts within any period of 30 seconds.If this limit is exceeded, 
%%                         the supervisor terminates itself and all its child processes and propagates the 
%%                         failure up the supervision tree. 4 restarts per hour (3600 seconds) is often 
%%                         used in production systems, for dev, use 0 and 1 to catch the problems.
RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children} }.

