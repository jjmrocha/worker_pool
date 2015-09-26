%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(worker_pool_sup).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_pool/2,
  start_pool/3,
  stop_pool/1]).

-spec start_pool(PoolName, StartFunction) -> supervisor:startlink_ret() when
  PoolName :: atom(),
  StartFunction :: {Module, Function, Args},
  Module :: atom(),
  Function :: atom(),
  Args :: list().
start_pool(PoolName, {Module, Function, Args}) when is_atom(PoolName) 
    andalso is_atom(Module) 
    andalso is_atom(Module) 
    andalso is_list(Args) ->
  WorkerCount = erlang:system_info(schedulers),
  start_pool(PoolName, WorkerCount, {Module, Function, Args}).

-spec start_pool(PoolName, WorkerCount, StartFunction) -> supervisor:startlink_ret() when
  PoolName :: atom(),
  WorkerCount :: integer(),
  StartFunction :: {Module, Function, Args},
  Module :: atom(),
  Function :: atom(),
  Args :: list().
start_pool(PoolName, WorkerCount, {Module, Function, Args}) when is_atom(PoolName) 
    andalso is_integer(WorkerCount) 
    andalso is_atom(Module) 
    andalso is_atom(Module) 
    andalso is_list(Args) ->
  SuperName = supervisor_name(PoolName),
  supervisor:start_link({local, SuperName}, ?MODULE, [PoolName, WorkerCount, Module, Function, Args]).

-spec stop_pool(PoolName :: atom()) -> ok.
stop_pool(PoolName) when is_atom(PoolName) ->
  SuperName = supervisor_name(PoolName),
  case whereis(SuperName) of
    undefined -> ok;
    Pid ->
      unlink(Pid),
      exit(Pid, shutdown),
      ok
  end.

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([PoolName, WorkerCount, Module, Function, Args]) ->
  Pool = {pool_name(PoolName), {worker_pool, start_link, [PoolName]}, permanent, infinity, worker, [worker_pool]},
  PoolSup = {worker_supervisor_name(PoolName), {worker_sup, start_link, [PoolName, WorkerCount, Module, Function, Args]}, permanent, infinity, supervisor, [worker_sup]},
  Procs = [Pool, PoolSup],
  {ok, {{one_for_one, 10, 60}, Procs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

supervisor_name(PoolName) ->
  list_to_atom(atom_to_list(PoolName) ++ "_sup").

pool_name(PoolName) ->
  atom_to_list(PoolName) ++ "_pool".

worker_supervisor_name(PoolName) ->
  list_to_atom(atom_to_list(PoolName) ++ "_worker_sup").
