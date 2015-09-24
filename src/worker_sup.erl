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

-module(worker_sup).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/5]).

start_link(PoolName, WorkerCount, Module, Function, Args) ->
	supervisor:start_link(?MODULE, [PoolName, WorkerCount, Module, Function, Args]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([PoolName, WorkerCount, Module, Function, Args]) ->
	Workers = [{worker(PoolName, N), {worker_pool_worker, start_worker, [PoolName, Module, Function, Args]}, permanent, infinity, worker, [worker_pool_worker]} || N <- lists:seq(1, WorkerCount)],
	{ok, {{one_for_one, 10, 60}, Workers}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

worker(PoolName, Id) ->
	list_to_atom(atom_to_list(PoolName) ++ "_worker_" ++ integer_to_list(Id)).
