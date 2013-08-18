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
-module(ebi_store).
-export([
    add_model/2, get_model/2, get_model/3, get_models/2, get_model_representation/4
    %add_simulation/2, set_simulation_status/2, set_simulation_target/2, get_simulation/1, get_simulations/1
]).
-include("ebi.hrl").

%% =============================================================================
%%  Callback definitions.
%% =============================================================================

-callback add_model(
        #model{},
        StoreArgs :: term()
    ) -> ok.

-callback get_model(
        model_id(),
        model_ref() | latest,
        StoreArgs :: term()
    ) -> {ok, #model{}}.

-callback get_model(
        Query :: all,
        StoreArgs :: term()
    ) -> {ok, [#model{}]}.

-callback get_model_representation(
        model_id(),
        model_ref(),
        [model_type()],
        StoreArgs :: term()
    ) -> undefined | term().

%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Add new model to a database.
%%  This function ignores duplicate inserts (duplicates not inserted).
%%
-spec add_model(store_ref(), #model{})
        -> ok.

add_model({StoreMod, StoreArgs}, Model) ->
    StoreMod:add_model(Model, StoreArgs).



%%
%%  Returns last version of a model.
%%
-spec get_model(store_ref(), model_id())
        -> {ok, #model{}}.

get_model(StoreRef, ModelId) ->
    get_model(StoreRef, ModelId, latest).

%%
%%  Returns a model of the specified version.
%%
-spec get_model(store_ref(), model_id(), model_ref())
        -> {ok, #model{}}.

get_model({StoreMod, StoreArgs}, ModelId, ModelRef) ->
    StoreMod:get_model(ModelId, ModelRef, StoreArgs).


%%
%%
%%
-spec get_models(store_ref(), all)
        -> {ok, [#model{}]}.

get_models({StoreMod, StoreArgs}, Query) ->
    StoreMod:get_models(Query).


%%
%%
%%
-spec get_model_representation(store_ref(), model_id(), model_ref(), [model_type()])
        -> undefined | term().

get_model_representation({StoreMod, StoreArgs}, ModelId, ModelRef, ModelRepTypes) ->
    StoreMod:get_model_representation(ModelId, ModelRef, ModelRepTypes, StoreArgs).


%%
%%  TODO.
%%
%-spec add_simulation(store_ref(), #simulation{})
%        -> undefined | term().
%
%add_simulation({StoreMod, StoreArgs}, Simulation) ->
%     ok.


%% set_simulation_status(_Simulation, _Status) ->
%%     ok.
%%
%% set_simulation_target(_Simulation, _Target) ->
%%     ok.
%%
%% get_simulation(_SimulationId) ->
%%     ok.
%%
%% get_simulations(_Query = {pending, _Count}) ->
%%     ok.


