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
