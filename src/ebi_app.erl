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
-module(ebi_app).
-behaviour(application).
-export([startup/0, shutdown/0, install/1]).
-export([start/2, stop/1]).
-include("bio_ers_queue_mifcl2.hrl").


%% =============================================================================
%%  Interface functions.
%% =============================================================================

%%
%%  Start the application from the command line.
%%  Example:
%%      erl -pa ebin -s bio_ers_app startup
%%
startup() ->
    application:start(ebi).

%%
%%  Sutdown the application.
%%
shutdown() ->
    application:stop(ebi).



%%
%%  Install the MNesia DB.
%%
-spec install([atom()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    {atomic, ok} = mnesia:create_table(ebi_mifcl2_sim, [
        {type, set},
        {attributes, record_info(fields, ebi_mifcl2_sim)},
        {disc_copies, Nodes}
    ]),
    rpc:multicall(Nodes, application, stop, [mnesia]).



%% =============================================================================
%%  Callbacks
%% =============================================================================

start(normal, _Args) ->
    MifCl2Name = mifcl2,
    MifCl2Cfg = {bio_ers_queue_mifcl2, MifCl2Name, [
        {partition,
            long,                   % name
            "uosis.mif.vu.lt",      % host
            22,                     % port
            "karolis",              % user
            "/home/karolis/CODE/kape1395.biosensor.solver-2D/src/bio_ers/etc/ssh", % local user dir
            "/users3/karolis/PST/bin/cluster",                                    % cluster command
            "long",                 % partition name
            1000,                   % concurent tasks
            60000                   % status check ms.
        }
    ]},
    Config = {ebi_config, [
        {queue, MifCl2Name, {bio_ers_queue_mifcl2_sup, start_link, [MifCl2Cfg]}}
    ]},
    bio_ers_sup:start_link(Config).
 
stop(_State) ->
    ok.

