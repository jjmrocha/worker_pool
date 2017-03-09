**DEPRECATED**

*Use async_pool from [jjmrocha/async](https://github.com/jjmrocha/async) instead*

===============

**worker_pool**
===============
*Erlang abstract worker pool*


Worker_pool is a simple permanent pool of erlang processes heavily inspired on [bfrog/hottub](https://github.com/bfrog/hottub) implementation.



Installation
------------

Using rebar:

```erlang
{deps, [
	{worker_pool, ".*", {git, "https://github.com/jjmrocha/worker_pool.git", "master"}}
]}.
```


Starting and stopping pools
---------------------------

The module ```worker_pool_sup``` provides 2 functions to create a worker pool (```start_pool/2``` and ```start_pool/3```) and a function to drop the pool ```stop_pool/1```.

```erlang
%% Starts a pool with a process for each core.
worker_pool_sup:start_pool(PoolName, StartFunction) -> supervisor:startlink_ret() when
  PoolName :: atom(),
  StartFunction :: {Module, Function, Args},
  Module :: atom(),
  Function :: atom(),
  Args :: list().
  
worker_pool_sup:start_pool(PoolName, WorkerCount, StartFunction) -> supervisor:startlink_ret() when
  PoolName :: atom(),
  WorkerCount :: integer(),
  StartFunction :: {Module, Function, Args},
  Module :: atom(),
  Function :: atom(),
  Args :: list().
  
worker_pool_sup:stop_pool(PoolName :: atom()) -> ok.    
```



Worker instances
----------------

The module ```worker_pool``` provides the following functions to interact with the worker instances.

```erlang
worker_pool:cast(PoolName :: atom(), Msg :: term()) -> ok.

worker_pool:call(PoolName :: atom(), Msg :: term()) -> term().

worker_pool:call(PoolName :: atom(), Msg :: term(), Timeout :: integer()) -> term().

worker_pool:multicast(PoolName :: atom(), Msg :: term()) -> ok.   
```

Additionally all messages received by the ```worker_pool``` process will be forwarded to one of the worker pool processes.



Using worker_pool
-----------------

### Worker implementation

Create a module with your worker's logic, the following example uses an ```gen_server``` but can be any type of process, the only condition is that the start function must return ```{ok, Pid :: pid()}```.

```erlang
-module(math).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([add/2, print/1]).

%% Starts a worker process, each worker process will be a gen_server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Sends a call request to one off the worker processes
add(A, B) ->
    worker_pool:call(?MODULE, {add, A, B}).

%% Sends a cast message for all worker processes
print(Msg) ->
    worker_pool:multicast(?MODULE, Msg).

init([]) ->
    {ok, none}.

handle_call({add, A, B}, _From, State) ->
    Reply = A + B,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("~p: ~p~n", [self(), Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### Start pool

Starting a pool will start the following processes:
* 1 worker_pool_sup process - Registered as "<pool_name>_sup", i.e. in this case the worker poll name is "math", so the supervisor name will "math_sup"
* 1 worker_pool process (under worker_pool_sup) - Registered as "<pool_name>", i.e. in this case the name will be "math"
* 1 worker_sup process (under worker_pool_sup)
* n worker processes (under worker_sup)

```erlang
1> worker_pool_sup:start_pool(math, {math, start_link, []}).

=INFO REPORT==== 26-Sep-2015::23:53:15 ===
math starting on [<0.36.0>]...
{ok,<0.35.0>}
```

### Using the pool

```erlang
2> math:add(3, 4).
7
3> math:print("I love Erlang").
<0.40.0>: "I love Erlang"
<0.39.0>: "I love Erlang"
<0.38.0>: "I love Erlang"
<0.41.0>: "I love Erlang"
ok
```

### Stopping the pool

```erlang
4> worker_pool_sup:stop_pool(math).
ok
```
