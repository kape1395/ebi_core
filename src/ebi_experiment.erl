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
-module(ebi_experiment).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]). % Callbacks
-export([init/2]). % State callbacks
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%  @doc Initialize the FSM.
%%
init(_Args) ->
    {ok, init, #state{}}.

%%
%%
%%
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

%%
%%
%%
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%
%%
%%
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%
%%
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%
%%  State: init
%%
init(_Event, StateData) ->
    {next_state, init, StateData}.
