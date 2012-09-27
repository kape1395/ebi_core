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
%% @doc Main interface of the `bio_ers' application.
%% @headerfile "bio_ers.hrl"
%%
-module(bio_ers).
-behaviour(gen_server).
-export([start_link/0, stop/0, get_id/1]).
-export([test/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include("bio_ers.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Initialization.
%%

start_link() ->
    gen_server:start_link({local, bio_ers}, bio_ers, [], []).

stop() ->
    gen_server:cast(bio_ers, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public interface.
%%

test() ->
    gen_server:call(bio_ers, test).


%%
%%  @doc Generates SHA1 ID for the specified simulation.
%%  @spec get_id(Simulation::#simulation{}) -> string()
%%
get_id(Simulation) when is_record(Simulation, simulation) ->
    #simulation{version = Version, model = Model, params = Params} = Simulation,
    Key = {Version, Model, lists:sort(Params)},
    get_id(erlang:term_to_binary(Key));

get_id(Model) when is_record(Model, model) ->
    #model{type = Type, definition = Definition} = Model,
    Key = {Type, Definition},
    get_id(erlang:term_to_binary(Key));

get_id(unique) ->
    get_id(erlang:term_to_binary(erlang:make_ref()));

get_id(Binary) when is_binary(Binary) ->
    SHA = crypto:sha(Binary),
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(SHA)]).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

%%
%%  Sync calls.
%%
handle_call(test, _From, State) ->
    {reply, "This is a test", State}.

%%
%%  Async calls.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

