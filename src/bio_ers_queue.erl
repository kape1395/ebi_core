%
% Copyright 2012 Karolis Petrauskas
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
%%  @doc This module defined bio_ers_queue behaviour and therefore defines
%%  an interface for all the queue implementations.
%%
-module(bio_ers_queue).
-behaviour(gen_server).
-export([start_link/2, start_link/3, submit/2, cancel/2, delete/2, result/2, status/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]). % Callbacks
-include("bio_ers.hrl").


%%  module and module's state
-record(state, {m, ms}).


%% =============================================================================
%%  Interface for the callback module.
%% =============================================================================


%%
%%  See gen_server:init/1 for more details.
%%
-callback init(term()) -> term().

%%
%%  See gen_server:terminate/2 for more details.
%%
-callback terminate(term(), term()) -> term().

%%
%%
%%
-callback handle_submit(Simulation :: #simulation{}, State :: term()) ->
    {ok, NewState :: term()}.

%%
%%
%%
-callback handle_cancel(Simulation :: #simulation{}, State :: term()) ->
    {ok, NewState :: term()}.

%%
%%
%%
-callback handle_delete(Simulation :: #simulation{}, State :: term()) ->
    {ok, NewState :: term()}.

%%
%%
%%
-callback handle_result(Simulation :: #simulation{}, State :: term()) ->
    {ok, Data :: binary(), NewState :: term()} |
    {error, Reason :: atom(), NewState :: term()}.

%%
%%
%%
-callback handle_status(Simulation :: #simulation{}, State :: term()) ->
    {ok, Status :: atom(), NewState :: term()}.



%% =============================================================================
%% Queue interface.
%% =============================================================================


%%
%%  @doc
%%  See {@link gen_server:start_link/3} for more details on result.
%%
-spec start_link(module(), term()) -> {ok, pid()} | term().
start_link(Module, Args) ->
	gen_server:start_link(?MODULE, {Module, Args}, []).


%%
%%  @doc
%%  See {@link gen_server:start_link/4} for more details on name and result.
%%
-spec start_link(term(), module(), term()) -> {ok, pid()} | term().
start_link(Name, Module, Args) ->
	gen_server:start_link(Name, ?MODULE, {Module, Args}, []).


%%
%%
%%
-spec submit(pid(), #simulation{}) -> ok.
submit(PID, Simulation) ->
	ok = gen_server:cast(PID, {submit, Simulation}).


%%
%%
%%
-spec cancel(pid(), #simulation{}) -> ok.
cancel(PID, Simulation) ->
	ok = gen_server:cast(PID, {cancel, Simulation}).


%%
%%
%%
-spec delete(pid(), #simulation{}) -> ok.
delete(PID, Simulation) ->
	ok = gen_server:cast(PID, {delete, Simulation}).


%%
%%
%%
-spec result(pid(), #simulation{}) -> {ok, Data::binary()} | {error, Reason :: running | failed | undefined}.
result(PID, Simulation) ->
	gen_server:call(PID, {result, Simulation}).


%%
%%
%%
-spec status(pid(), #simulation{}) -> {ok, Status :: pending | running | done | failed | undefined}.
status(PID, Simulation) ->
	gen_server:call(PID, {status, Simulation}).



%% =============================================================================
%%  Callbacks.
%% =============================================================================

%%
%%
%%
init({Module, Args}) ->
    case Module:init(Args) of
        {ok, MS}       -> {ok, #state{m = Module, ms = MS}};
        {ok, MS, Opts} -> {ok, #state{m = Module, ms = MS}, Opts};
        Else -> Else
    end.


%%
%%
%%
terminate(Reason, #state{m = M, ms = MS}) ->
    M:terminate(Reason, MS).


%%
%% Sync calls.
%%
handle_call({result, Simulation}, _From, State = #state{m = M, ms = MS}) ->
    case M:handle_result(Simulation, MS) of
        {ok,    Data,   NewMS} -> {reply, {ok,    Data},   State#state{ms = NewMS}};
        {error, Reason, NewMS} -> {reply, {error, Reason}, State#state{ms = NewMS}}
    end;

handle_call({status, Simulation}, _From, State = #state{m = M, ms = MS}) ->
    {ok, Status, NewMS} =  M:handle_status(Simulation, MS),
    {reply, {ok, Status}, State#state{ms = NewMS}};

handle_call(Msg, From, State = #state{m = M, ms = MS}) ->
    case erlang:function_exported(M, handle_call, 3) of
        true ->
            case M:handle_call(Msg, From, MS) of
                {reply, Reply, NewMS}         -> {reply, Reply, State#state{ms = NewMS}};
                {reply, Reply, NewMS, Opts}   -> {reply, Reply, State#state{ms = NewMS}, Opts};
                {noreply, NewMS}              -> {noreply, State#state{ms = NewMS}};
                {noreply, NewMS, Opts}        -> {noreply, State#state{ms = NewMS}, Opts};
                {stop, Reason, Reply, NewMS}  -> {stop, Reason, Reply, State#state{ms = NewMS}};
                {stop, Reason, NewMS}         -> {stop, Reason, State#state{ms = NewMS}}
            end;
        _ ->
            {stop, badarg, State}
    end.



%%
%%  Async calls.
%%
handle_cast({submit, Simulation}, State = #state{m = M, ms = MS}) ->
    {ok, NewMS} = M:handle_submit(Simulation, MS),
    {noreply, State#state{ms = NewMS}};

handle_cast({cancel, Simulation}, State = #state{m = M, ms = MS}) ->
    {ok, NewMS} = M:handle_cancel(Simulation, MS),
    {noreply, State#state{ms = NewMS}};

handle_cast({delete, Simulation}, State = #state{m = M, ms = MS}) ->
    {ok, NewMS} = M:handle_delete(Simulation, MS),
    {noreply, State#state{ms = NewMS}};

handle_cast(Msg, State = #state{m = M, ms = MS}) ->
    case erlang:function_exported(M, handle_cast, 2) of
        true ->
            case M:handle_cast(Msg, MS) of
                {noreply, NewMS}       -> {noreply, State#state{ms = NewMS}};
                {noreply, NewMS, Opts} -> {noreply, State#state{ms = NewMS}, Opts};
                {stop, Reason, NewMS}  -> {stop, Reason, State#state{ms = NewMS}}
            end;
        _ ->
            {noreply, State}
    end.


%%
%%  Unknown messages.
%%
handle_info(Msg, State = #state{m = M, ms = MS}) ->
    case erlang:function_exported(M, handle_info, 2) of
        true ->
            case M:handle_info(Msg, MS) of
                {noreply, NewMS}       -> {noreply, State#state{ms = NewMS}};
                {noreply, NewMS, Opts} -> {noreply, State#state{ms = NewMS}, Opts};
                {stop, Reason, NewMS}  -> {stop, Reason, State#state{ms = NewMS}}
            end;
        _ ->
            {noreply, State}
    end.


%%
%%  Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

