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
%% @doc Main interface of the `ebi' application.
%% @headerfile "ebi.hrl"
%%
-module(ebi).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, get_id/1, submit/1, cancel/1, delete/1, result/1, status/1]).
-export([experiment_result/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include("ebi.hrl").


%% =============================================================================
%%  Public interface.
%% =============================================================================

%%
%%  Start the server.
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%%  @doc Generates SHA1 ID for the specified simulation.
%%  @spec get_id(Simulation::#simulation{}) -> string()
%%
get_id(Simulation) when is_record(Simulation, simulation) ->
    #simulation{version = Version, model = Model, params = Params} = Simulation,
    Key = {Version, Model, lists:sort(Params)},
    get_id(erlang:term_to_binary(Key));

get_id(Model) when is_record(Model, model) ->
    ebi_model:get_ref(Model);

get_id(unique) ->
    ebi_utils:sha1sum(erlang:term_to_binary(erlang:make_ref()));

get_id(Binary) when is_binary(Binary) ->
    ebi_utils:sha1sum(Binary).


%%
%%  Starts single simulation.
%%
-spec submit(#simulation{})
        -> ok.

submit(Simulation) when is_record(Simulation, simulation) ->
    SimulationWithId = Simulation#simulation{id = get_id(Simulation)},
    ok = ebi_store:add_simulation(SimulationWithId),
    ok = ebi_queue:submit(SimulationWithId).

submit(ModelCB, ExpParams) ->
    {ok, StoreRef} = ebi_model:store_ref(ModelCB),
    SimParams = ebi_model:exp_to_sim(ModelCB, ExpParams),
    Simulation = #simulation{
        model = Model,
        params = SimParams
    },

    ebi_queue:enqueue_if_missing(ModelCB),
    ok. % TODO

%%
%%  TODO: Cancels the simulation.
%%
cancel(#simulation{}) ->
    ok.


%%
%%  TODO: Delete simulation and its results.
%%
delete(#simulation{}) ->
    ok.


%%
%%  TODO: Get simulation results.
%%
result(#simulation{}) ->
    ok.


%%
%%  TODO: Get simulation status.
%%
status(#simulation{}) ->
    ok.


perform(Model, SeriesSpec) ->
    ok.


%%
%%
%%
experiment_result({ModelCBMod, ModelCBArgs}, ExpSymbols, ResultSpec) ->
    case ebi_queue:enqueue() of
        {done, Ref} ->
            ok;
        {wait, Ref} ->
            ok
    end.



%% =============================================================================
%%  Internal state.
%% =============================================================================

-record(state, {
}).



%% =============================================================================
%%  Callbacks for gen_server
%% =============================================================================

%%
%%  Initialization.
%%
init(_Args) ->
    {ok, #state{}}.


%%
%%  Sync calls.
%%
handle_call(_Message, _From, State) ->
    {reply, undefined, State}.


%%
%%  Async calls.
%%
handle_cast(_, State) ->
    {noreply, State}.


%%
%%  Other messages.
%%
handle_info(_, State) ->
    {noreply, State}.


%%
%%  Code upgrade.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%%  Cleanup.
%%
terminate(_Reason, _State) ->
    ok.



%% =============================================================================
%%  Internal functions.
%% =============================================================================
