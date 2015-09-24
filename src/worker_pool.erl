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
-module(worker_pool).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
	add_worker/2,
	cast/2,
	call/2,
	call/3]).

start_link(Name) ->
	gen_server:start_link(?MODULE, [Name], []).

add_worker(Name, Pid) ->
	gen_server:call(Name, {add_worker, Pid}).

cast(Name, Msg) ->
	gen_server:cast(Name, Msg).

call(Name, Msg) ->
	case gen_server:call(Name, {next_worker}) of
		{ok, Pid} -> gen_server:call(Pid, Msg);
		Other -> {error, Other}
	end.

call(Name, Msg, Timeout) ->
	case gen_server:call(Name, {next_worker}) of
		{ok, Pid} -> gen_server:call(Pid, Msg, Timeout);
		Other -> {error, Other}
	end.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
init([Name]) ->
	process_flag(trap_exit, true),	
	erlang:register(Name, self()),
	error_logger:info_msg("~p starting on [~p]...\n", [Name, self()]),
	{ok, []}.

%% handle_call/3
handle_call({next_worker}, _From, State=[]) ->
	{reply, empty_pool, State};
handle_call({next_worker}, _From, State) ->
	{Worker, NewState} = next_worker(State),
	Reply = {ok, Worker},
	{reply, Reply, NewState};
handle_call({add_worker, Worker}, _From, Workers) ->
	erlang:monitor(process, Worker),
	{reply, ok, [Worker|Workers]};
handle_call(_Request, _From, State) ->
	{reply, invalid_request, State}.

%% handle_cast/2
handle_cast(_Msg, State=[]) ->
	{noreply, State};
handle_cast(Msg, State) ->
	{Worker, NewState} = next_worker(State),
	gen_server:cast(Worker, Msg),
	{noreply, NewState}.

%% handle_info/2
handle_info({'DOWN', _, _, Worker, _}, State) ->
	NewState = delete_worker(Worker, State),
	{noreply, NewState};
handle_info(_Info, State=[]) ->
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

next_worker([Worker|T]) ->
	Workers = T ++ [Worker],
	{Worker, Workers}.

delete_worker(Worker, Workers) ->
	lists:delete(Worker, Workers).