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
-module(bio_ers_testsuite).
-export([run/1]).


%%
%%  @doc Executes tests in specified modules and exits with
%%  non 0, if some tests are failing. This is intended to
%%  run from the Makefile, the check target.
%%
%%  @spec run(TestModules::list()) -> ok
%%
run(TestModules) ->
    case eunit:test(TestModules) of
        ok ->
            io:format("#~n# Tests passed.~n#~n"),
            init:stop();
        error ->
            io:format("#~n# Tests failed.~n#~n"),
            init:stop(1);
        _ ->
            io:format("#~n# Something wrong.~n#~n"),
            init:stop(2)
    end.

