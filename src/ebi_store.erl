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
%% @doc An entry point to the data store managing biosensor simulations
%% and related entities.
%%

-module(ebi_store).
-behaviour(gen_server).
-export([start_link/0]).
-export([install/1, wait_for_tables/1]).
-export([
    add_model/1, get_model/1, get_model/2, get_models/1,
    add_simulation/1, get_model_representation/3
    %, set_simulation_status/2, set_simulation_target/2, get_simulation/1, get_simulations/1
]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include("ebi.hrl").

-define(ATTRS(R), {attributes, record_info(fields, R)}).
-define(STORE_MODEL_TYPE, ebi_sbml_v1).


%%
%%
%%
-record(ebi_store_biosensor, {
    id          :: string(),
    name        :: string(),
    description :: string()
}).


%%
%%  Model, as it is visible to a user. The model is identified via its ID.
%%  Contents of the model can change over time. Each version of the model is
%%  defined by the `#ebi_store_model_def{}`.
%%
-record(ebi_store_model, {
    id          :: model_id(),
    name        :: string(),
    description :: string(),
    status      :: model_status(),
    definitions = [] :: [model_ref()]
}).


%%
%%  Model definition. Definitions are referenced from models and simulations
%%  and they cannot be changed ever time. The definitions are non-variable to
%%  ensure traceability in the simulations and are defined in the intarnal EBI format.
%%
-record(ebi_store_model_def, {
    ref         :: model_ref(),
    content     :: term(),              % TODO: WTF
    params      :: [model_param()],
    created     :: timestamp(),
    created_by  :: string()
}).


%%
%%  External representation of a model definition.
%%  The representations are created on demand using available converters.
%%
-record(ebi_store_model_rep, {
    model_ref   :: model_ref(),
    model_id    :: model_id(),
    model_type  :: model_type(),
    content     :: term()               % TODO: WTF
}).


%%
%%
%%
-record(ebi_store_tag, {
    name        :: string(),
    description :: string(),
    biosensors  :: [string()],
    models      :: [model_id()]
}).


%%
%%  Counter table.
%%
-record(ebi_store_counter, {
    key         ::  atom(),
    value       ::  integer()
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
            ebi_store_biosensor,
            ebi_store_model,
            ebi_store_model_def,
            ebi_store_model_rep,
            ebi_store_tag,
            ebi_store_counter
        ], Timeout).

%%
%%  Creates mnesia tables.
%%
create_tables(Nodes) ->
    DefOptDC = {disc_copies, Nodes},
    OK = {atomic, ok},
    OK = mnesia:create_table(ebi_store_biosensor,   [{type, set},  ?ATTRS(ebi_store_biosensor), DefOptDC]),
    OK = mnesia:create_table(ebi_store_model,       [{type, set},  ?ATTRS(ebi_store_model),     DefOptDC]),
    OK = mnesia:create_table(ebi_store_model_def,   [{type, set},  ?ATTRS(ebi_store_model_def), DefOptDC]),
    OK = mnesia:create_table(ebi_store_model_rep,   [{type, bag},  ?ATTRS(ebi_store_model_rep), DefOptDC]),
    OK = mnesia:create_table(ebi_store_tag,         [{type, set},  ?ATTRS(ebi_store_tag),       DefOptDC]),
    OK = mnesia:create_table(ebi_store_counter,     [{type, set},  ?ATTRS(ebi_store_counter),   DefOptDC]),
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
    #model{
        id          = OldModelId,
        ref         = _OldModelRef,
        name        = Name,
        description = Description,
        status      = Status,
        changed     = Changed,
        changed_by  = ChangedBy,
        definition  = ModelDef,
        parameters  = ModelParams,
        representations = Representations
    } = Model,
    Activity = fun () ->
        ModelRef = ebi_model:get_ref(Model),
        {ModelId, Definitions} = case mnesia:read(ebi_store_model, OldModelId) of
            [] ->
                {new_id(model), [ModelRef]};
            [#ebi_store_model{definitions = OldDefinitions}] ->
                {OldModelId, [ModelRef | OldDefinitions]}
        end,
        ok = mnesia:write(#ebi_store_model{
            id = ModelId,
            name = Name,
            description = Description,
            status = Status,
            definitions = Definitions
        }),
        ok = mnesia:write(#ebi_store_model_def{
            ref = ModelRef,
            created = Changed,
            created_by = ChangedBy,
            content = ModelDef,
            params = ModelParams
        }),
        WriteRepFun = fun ({RT, RC}) ->
            ok = mnesia:write(#ebi_store_model_rep{
                model_ref = ModelRef,
                model_id = ModelId,
                model_type = RT,
                content = RC
            })
        end,
        lists:foreach(WriteRepFun, Representations),
        {ok, ModelId, ModelRef}
    end,
    mnesia:activity(transaction, Activity).



%%
%%  Returns last version of a model.
%%
-spec get_model(model_id()) -> {ok, #model{}}.
get_model(ModelId) ->
    get_model(ModelId, latest).

%%
%%  Returns a model of the specified version.
%%
-spec get_model(model_id(), model_ref()) -> {ok, #model{}}.
get_model(ModelId, ModelRef) ->
    Activity = fun () ->
        [Model = #ebi_store_model{definitions = Defs}] = mnesia:read(ebi_store_model, ModelId),
        ModelDef = case {ModelRef, Defs} of
            {latest, []}                 -> undefined;
            {latest, [LastModelRef | _]} -> read_model_def(ModelId, LastModelRef);
            _                            -> read_model_def(ModelId, ModelRef)
        end,
        {ok, fill_model(Model, ModelDef)}
    end,
    mnesia:activity(transaction, Activity).


%%
%%
%%
-spec get_models(all) -> {ok, [#model{}]}.
get_models(_Query = all) ->
    Activity = fun () ->
        ModelFun = fun (Model = #ebi_store_model{id = ModelId, definitions = ModelDefs}) ->
            ModelDef = case ModelDefs of
                []                   -> undefined;
                [LatestModelRef | _] -> read_model_def(ModelId, LatestModelRef)
            end,
            fill_model(Model, ModelDef)
        end,
        Models = mnesia:match_object(#ebi_store_model{_='_'}),
        lists:map(ModelFun, Models)
    end,
    Result = mnesia:activity(transaction, Activity),
    {ok, Result}.


%%
%%
%%
-spec get_model_representation(model_id(), model_ref(), [model_type()])
    -> undefined | term().

get_model_representation(_ModelId, ModelRef, ModelRepTypes) ->
    Reps = mnesia:dirty_read(ebi_store_model_rep, ModelRef),
    SearchFun = fun (MRT, Found) ->
        RepOfType = [ RepContent
            || #ebi_store_model_rep{model_type = RepType, content = RepContent} <- Reps,
            RepType =:= MRT
        ],
        case {RepOfType, Found} of
            {_, F} when F =/= undefined -> F;
            {[], undefined} -> undefined;
            {[R], undefined} -> R
        end
    end,
    lists:foldl(SearchFun, undefined, ModelRepTypes).


%%
%%  TODO.
%%
add_simulation(_Simulation) ->
     ok.


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

read_model_def(_ModelId, ModelRef) ->
    ModelDefs = mnesia:read(ebi_store_model_def, ModelRef),
    case ModelDefs of
        [] -> undefined;
        [MD] -> MD
    end.

fill_model(Model, ModelDef) ->
    #ebi_store_model{
        id = ModelId,
        name = ModelName,
        description = Description,
        status = ModelStatus
    } = Model,
    case ModelDef of
        undefined ->
            ModelRef = undefined,
            Changed = undefined,
            ChangedBy = undefined,
            Definition = undefined,
            Parameters = undefined;
        _ ->
            #ebi_store_model_def{
                ref = ModelRef,
                created = Changed,
                created_by = ChangedBy,
                content = Definition,
                params = Parameters
            } = ModelDef
    end,
    #model{
        id = ModelId,
        ref = ModelRef,
        name = ModelName,
        description = Description,
        status = ModelStatus,
        changed = Changed,
        changed_by = ChangedBy,
        definition = Definition,
        parameters = Parameters,
        representations = []
    }.


new_id(model) ->
    Counter = mnesia:dirty_update_counter(ebi_store_counter, model, 1),
    [ $M, $- | integer_to_list(Counter)].


