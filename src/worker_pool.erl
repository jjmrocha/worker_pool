%%
%% Copyright 2015-16 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(worker_pool).

-behaviour(gen_server).

-define(EMPTY, []).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
	add_worker/2,
	cast/2,
	call/2,
	call/3,
	multicast/2]).

start_link(PoolName) ->
	gen_server:start_link({local, PoolName}, ?MODULE, [PoolName], []).

add_worker(PoolName, Pid) ->
	gen_server:call(PoolName, {add_worker, Pid}).

-spec cast(PoolName :: atom(), Msg :: term()) -> ok.
cast(PoolName, Msg) ->
	gen_server:cast(PoolName, Msg).

-spec call(PoolName :: atom(), Msg :: term()) -> term().
call(PoolName, Msg) ->
	with_worker(PoolName, fun(Worker) -> 
			gen_server:call(Worker, Msg) 
		end).

-spec call(PoolName :: atom(), Msg :: term(), Timeout :: integer()) -> term().
call(PoolName, Msg, Timeout) ->
	with_worker(PoolName, fun(Worker) -> 
			gen_server:call(Worker, Msg, Timeout)
		end).

-spec multicast(PoolName :: atom(), Msg :: term) -> ok.
multicast(PoolName, Msg) ->
	gen_server:cast(PoolName, {multicast, Msg}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
init([PoolName]) ->
	error_logger:info_msg("~p starting on [~p]...\n", [PoolName, self()]),
	{ok, ?EMPTY}.

%% handle_call/3
handle_call({next_worker}, _From, State=?EMPTY) ->
	{reply, empty_pool, State};
handle_call({next_worker}, _From, State) ->
	{Worker, NewState} = next_worker(State),
	Reply = {ok, Worker},
	{reply, Reply, NewState};
handle_call({add_worker, Worker}, _From, State) ->
	erlang:monitor(process, Worker),
	NewState = add_worker_to_list(Worker, State),
	{reply, ok, NewState};
handle_call(_Request, _From, State) ->
	{reply, invalid_request, State}.

%% handle_cast/2
handle_cast(_Msg, State=?EMPTY) ->
	{noreply, State};
handle_cast({multicast, Msg}, State) ->
	Workers = all_workers(State),
	lists:foreach(fun(Worker) -> 
				gen_server:cast(Worker, Msg)
		end, Workers),
	{noreply, State};
handle_cast(Msg, State) ->
	{Worker, NewState} = next_worker(State),
	gen_server:cast(Worker, Msg),
	{noreply, NewState}.

%% handle_info/2
handle_info({'DOWN', _, _, Worker, _}, State) ->
	NewState = delete_worker(Worker, State),
	{noreply, NewState};
handle_info(_Info, State=?EMPTY) ->
	{noreply, State};
handle_info(Info, State) ->
	{Worker, NewState} = next_worker(State),
	Worker ! Info,
	{noreply, NewState}.

%% terminate/2
terminate(_Reason, _State) ->
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

with_worker(PoolName, Fun) ->
	case gen_server:call(PoolName, {next_worker}) of
		{ok, Pid} -> Fun(Pid);
		Other -> {error, Other}
	end.

add_worker_to_list(Worker, Workers) ->
	case lists:member(Worker, Workers) of
		true -> Workers;
		false -> [Worker|Workers]
	end.

next_worker([Worker|T]) ->
	Workers = T ++ [Worker],
	{Worker, Workers}.

delete_worker(Worker, Workers) ->
	lists:delete(Worker, Workers).

all_workers(Workers) -> 
	Workers.
