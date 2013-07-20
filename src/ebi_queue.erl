%
% Copyright 2012-2013 Karolis Petrauskas
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%

%%
%%  Queue for simulations.
%%  TODO: Take representation types into account.
%%
-module(ebi_queue).
-behaviour(gen_server).
-export([start_link/0, submit/1, cancel/1, status/1, solver_ready/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include("ebi.hrl").


%% =============================================================================
%%  Public API.
%% =============================================================================


%%
%%  Start the queue.
%%
-spec start_link()
        -> {ok, pid()} | term().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%  Submit new simulation.
%%
-spec submit(#simulation{})
        -> ok.

submit(Simulation) ->
    ok = gen_server:cast(?MODULE, {submit, Simulation}).


%%
%%  TODO: Remove simulation from the queue.
%%
-spec cancel(#simulation{})
        -> ok.

cancel(Simulation) ->
    ok = gen_server:cast(?MODULE, {cancel, Simulation}).


%%
%%  TODO: Get simulation status.
%%
-spec status(#simulation{})
        -> {ok, Status :: pending | running | unknown}.

status(Simulation) ->
    gen_server:call(?MODULE, {status, Simulation}).


%%
%%  Solver asks for a task.
%%
-spec solver_ready(pid(), list())
        -> ok.

solver_ready(Solver, SupportedRepTypes) ->
    gen_server:cast(?MODULE, {solver_ready, Solver, SupportedRepTypes}).



%% =============================================================================
%%  Internal state.
%% =============================================================================

-record(state, {
    tasks,
    workers
}).



%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================

%%
%%
%%
init({}) ->
    erlang:process_flag(trap_exit, true),
    State = #state{
        tasks = queue:new(),
        workers = queue:new()
    },
    {ok, State}.


%%
%% Sync calls.
%%
handle_call(_Message, _From, State) ->
    {reply, undefiled, State}.


%%
%%  Async calls.
%%
handle_cast({submit, Simulation}, State = #state{tasks = Tasks, workers = Workers}) ->
    case queue:out(Workers) of
        {empty, Workers} ->
            NewTasks = queue:in(Simulation, Tasks),
            NewState = State#state{tasks = NewTasks};
        {{value, Worker}, NewWorkers} ->
            ok = solver:solve(Worker, Simulation),
            true = erlang:unlink(Worker),
            NewState = State#state{workers = NewWorkers}
    end,
    {noreply, NewState};

handle_cast({solver_ready, Solver, _SupportedRepTypes}, State = #state{tasks = Tasks, workers = Workers}) ->
    case queue:out(Tasks) of
        {empty, Tasks} ->
            NewWorkers = queue:in(Solver, Workers),
            true = erlang:link(Solver),
            NewState = State#state{workers = NewWorkers};
        {{value, Task}, NewTasks} ->
            ok = solver:solve(Solver, Task),
            NewState = State#state{tasks = NewTasks}
    end,
    {noreply, NewState}.


%%
%%  Other messages.
%%
handle_info({'EXIT', From, _Reason}, State = #state{workers = Workers}) ->
    WorkersList = queue:to_list(Workers),
    CleanedWorkers = lists:filter(fun(W) -> W =/= From end, WorkersList),
    NewState = State#state{
        workers = queue:from_list(CleanedWorkers)
    },
    {noreply, NewState}.


%%
%%  Upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%%  Cleanup.
%%
terminate(_Reason, _State) ->
    ok.


