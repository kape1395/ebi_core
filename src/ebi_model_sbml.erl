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
-module(ebi_model_sbml).
-include("ebi_model_sbml.hrl").
-export([ok/0]).

ok() ->
    #sbml{
        model = #sbml_model{
            id = i,
            name = n,
            list_of_compartments = [
                #sbml_compartment{
                    sbase = #sbml_sbase{sboTerm = a, annotation = [], metaid = metaid1, notes = []},
                    id = ok,
                    name = ok
                }
            ],
            list_of_species = [
            ],
            list_of_parameters = [
            ],
            list_of_reactions = [
            ],
            list_of_initial_assignments = [
            ]
        }
    }.
