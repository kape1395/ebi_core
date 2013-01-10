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
%% @doc An entry point to the data store managing biosensor simulations
%% and related entities.
%%

-module(ebi_store).
-behaviour(gen_server).
-export([start_link/0]).
-export([install/1, wait_for_tables/1]).
-export([
    add_model/1, get_model/1, get_models/1,
    add_simulation/1, set_simulation_status/2, set_simulation_target/2, get_simulation/1, get_simulations/1
]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include("ebi.hrl").

-define(ATTRS(R), {attributes, record_info(fields, R)}).
-define(STORE_MODEL_TYPE, ebi_sbml_v1).


%%
%%  Model definition. Definitions are references from models and simulations
%%  and they cannot be changed ever. The definitions are non-variable to
%%  ensure traceability in the simulations.
%%
-record(ebi_store_model_def, {
    ref         :: model_ref(),
    type        :: model_type(),
    content     :: term(),
    params      :: [Name :: string()]
}).

%%
%%  Model, as it is visible to a user. The model is identified via its ID.
%%  Contents of the model can change over time. Multiple records with the same ID can exist.
%%
-record(ebi_store_model, {
    id          :: model_id(),
    name        :: string(),
    description :: string(),
    status      :: model_status(),
    changed     :: calendar:timestamp(),
    definition  :: model_ref(),                             %% Model Definition
    mapping     :: [{From :: string(), To :: string()}]     %% Parameter mapping (model -> model def).
}).


%% =============================================================================
%%  "Static" API.
%% =============================================================================


%%
%%  Installs Mnesia DB files on all nodes in the cluster.
%%
%%      rm -rf priv/test-db
%%      erl -pa ebin/ -sname ebi_store_test -mnesia dir temp/test-db
%%      erl -pa ebin/ -sname ebi_store_test -config priv/test
%%          ebi_store:install([node()]).
%%
-spec install([atom()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_tables(Nodes),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

%%
%%  Waits for tables.
%%
-spec wait_for_tables(number()) -> ok | term().
wait_for_tables(Timeout) ->
    mnesia:wait_for_tables([
            ebi_store_model,
            ebi_store_model_def
        ], Timeout).

%%
%%  Creates mnesia tables.
%%
create_tables(Nodes) ->
    DefOptDC = {disc_copies, Nodes},
    OK = {atomic, ok},
    OK = mnesia:create_table(ebi_store_model,       [{type, bag},  ?ATTRS(ebi_store_model),     DefOptDC]),
    OK = mnesia:create_table(ebi_store_model_def,   [{type, set},  ?ATTRS(ebi_store_model_def), DefOptDC]),
    ok.



%% =============================================================================
%%  Public API.
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%%  Add new model to a database.
%%  This function ignores duplicate inserts (duplicates not inserted).
%%
-spec add_model(#model{}) -> ok.
add_model(Model) ->
    % TODO: Extract ModelDef here.
    % TODO #model{type = ?STORE_MODEL_TYPE, id = ModelId} = Model,
    ModelId = ebi:get_id(Model),
    Activity = fun () ->
        case mnesia:read(ebi_store_model, ModelId) of
            [] ->
                mnesia:write(#ebi_store_model{
                    id = ModelId,
                    status = valid,
                    changed = erlang:now()
                   %type = Type,
                   %definition = Definition
                });
            [_] -> ok
        end
    end,
    ok = mnesia:activity(transaction, Activity).


%%
%%
%%
-spec get_model(model_id()) -> {ok, #model{}}.
get_model(ModelId) ->
    % TODO: Join with Model Def.
    Activity = fun () ->
        mnesia:read(ebi_store_model, ModelId)
    end,
    [Record] = mnesia:activity(transaction, Activity),
    #ebi_store_model{} = Record,
    {ok, #model{}}.


%%
%%
%%
-spec get_models(all) -> {ok, [#model{}]}.
get_models(_Query = all) ->
    Activity = fun () ->
        mnesia:match_object(#ebi_store_model{_='_'})
    end,
    Mapping = fun (#ebi_store_model{}) ->
        #model{}    % TODO: Load all needed attributes.
    end,
    Records = mnesia:activity(transaction, Activity),
    {ok, lists:map(Mapping, Records)}.


add_simulation(_Simulation) ->
    ok.

set_simulation_status(_Simulation, _Status) ->
    ok.

set_simulation_target(_Simulation, _Target) ->
    ok.

get_simulation(_SimulationId) ->
    ok.

get_simulations(_Query = {pending, _Count}) ->
    ok.






%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Event, _From, State) ->
    {reply, undefined, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% =============================================================================
%%  Internal functions.
%% =============================================================================

