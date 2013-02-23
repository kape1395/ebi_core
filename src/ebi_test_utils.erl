%
% Copyright 2013 Karolis Petrauskas
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
-module(ebi_test_utils).
-export([
    configure/1, install_db/0, start_core/0, stop_core/0
]).


%% =============================================================================
%%  API functions.
%% =============================================================================


configure(RootDir) ->
    application:load(sasl),
    application:load(lager),
    application:load(mnesia),
    application:load(ebi_core),
    ok = application:set_env(sasl, sasl_error_logger, {file, lists:flatten("log/sasl-error.log")}),
    ok = application:set_env(sasl, errlog_type, error),
    ok = application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    ok = application:set_env(lager, error_logger_redirect, true),
    ok = application:set_env(mnesia, dir, lists:flatten(RootDir, "/data/mnesia")).


install_db() ->
    ebi_store:install([node()]).


start_core() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(lager),
    ok = application:start(sasl),
    ok = application:start(gproc),
    ok = application:start(xmerl),
    ok = application:start(mnesia),
    ok = application:start(ebi_core),
    ok.


stop_core() ->
    ok = application:stop(ebi_core),
    ok = application:stop(mnesia),
    ok = application:stop(xmerl),
    ok = application:stop(gproc),
    ok = application:stop(sasl),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler),
    ok = application:stop(lager),
    ok.

