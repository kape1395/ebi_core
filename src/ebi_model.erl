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
%%  Behaviour for implementing particular biosensor models.
%%
-module(ebi_model).
-export([get_store_ref/1, exp_to_sim/2]).
-export([set/3, symbol_name/1]).
-include("ebi.hrl").


%% =============================================================================
%%  Callback definitions.
%% =============================================================================

%%
%%
%%
-callback store_ref(
        Args :: term()
    ) ->
    {ok, store_ref()}.

%%
%%
%%
-callback model_ref(
        Args :: term()
    ) ->
    {ok, model_ref()}.

%%
%%  Convert experiment symbols to simulation symbols.
%%
-callback exp_to_sim(
        ExperimentSymbols :: {exp, list()},
        Args              :: term()
    ) ->
    SimulationSymbols :: {sim, list()}.


%%
%%  Format simulation symbol name.
%%
-callback symbol_name(
        Symbol :: term(),
        Args   :: term()
    ) ->
    iolist().



%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%
%%
get_store_ref(_ModelCB = {Mod, Args}) ->
    {ok, {_StoreMod, _StoreArgs}} = Mod:store_ref(Args).


%%
%%
%%
get_model(_ModelCB = {Mod, Args}) ->
    {ok, {ModelMod, ModelArgs}} = Mod:model_ref(Args),
    {ok, Model} = ModelMod:get_model(ModelArgs).


%%
%%
%%
exp_to_sim(_ModelCB = {Mod, Args}, ExpParams) ->
    Mod:exp_to_sim(ExpParams, Args).



%% =============================================================================
%%  API for model implementations.
%% =============================================================================

%%
%%  Set new or replace existing value for the specified symbol (parameter).
%%
set(Name, Value, {exp, Symbols}) ->
    {exp, lists:keystore(Name, 1, Symbols, {Name, Value})}.



%%
%%  Default implementation for the symbol -> iolist conversion.
%%
symbol_name({N, I}) when is_integer(I) ->
    symbol_name({N, integer_to_list(I)});

symbol_name({N, I}) when is_atom(N) ->
    symbol_name({erlang:atom_to_list(N), I});

symbol_name({N, I}) ->
    [N, $_, I].


