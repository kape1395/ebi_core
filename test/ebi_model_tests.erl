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
-module(ebi_model_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ebi.hrl").
-include("ebi_model.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Test descriptions
%%
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

main_test_() ->
    [
        ?setup(fun test_parsing_successful/1),
        ?setup(fun test_read_model/1)
    ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Setup functions
%%

start() ->
    ok.

stop(_) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Actual tests
%%

test_parsing_successful(_) ->
    {Status, _Model} = ebi_model:parse_file("../test/ebi_model_tests-CNT-2D.xml"),
    [?_assertEqual(ok, Status)].


test_read_model(_) ->
    #model{representations = [Type, _Content]} = ebi_model:read_model(
        "../test/ebi_model_tests-CNT-2D.xml",
        undefined),
    [?_assertEqual(kp1_xml, Type)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions
%%


test_1() ->
    #ebi_model{
        species = [
            #ebi_species{name = "S", description = ""},
            #ebi_species{name = "P", description = ""},
            #ebi_species{name = "Eo", description = ""},
            #ebi_species{name = "Er", description = ""}
        ],
        reactions = [
            #ebi_reaction{name = "R1", definition = #ebi_rdef_mm{
                substrate = "S",
                product = "P",
                vmax = "Vmax",
                km = "KM"
            }},
            #ebi_reaction{name = "R2", definition = #ebi_rdef_simple{
                reagents = [{"Er", 1}, {"S", 1}],
                products = [{"Eo", 1}, {"P", 1}],
                rateconst = "k1"
            }}
        ],
        compartments = []
    }.



