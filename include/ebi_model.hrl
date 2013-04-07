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
-ifndef(ebi_model_hrl).
-define(ebi_model_hrl, ok).


%%
%%  EBI native biosensor model definition.
%%  The model is designed by keeping some alignment between this model and SBML.
%%  Import/export functionality should be used when working with SBPM.
%%


%% =============================================================================
%%  Generic types.
%% =============================================================================

-type parameter()       :: string().
-type species()         :: string().
-type reaction()        :: string().
-type compartment()     :: string().

-record(ebi_species, {
    name                :: species(),
    description         :: string()
}).


%%
%%  Reaction definitions.
%%
-record(ebi_rdef_simple, {
    reagents = []       :: [{species(), number()}],
    products = []       :: [{species(), number()}],
    rateconst           :: parameter()
}).
-record(ebi_rdef_fast, {
    reagents = []       :: [{species(), number()}],
    products = []       :: [{species(), number()}]
}).
-record(ebi_rdef_mm, {
    substrate           :: species(),
    product             :: species(),
    vmax                :: parameter(),
    km                  :: parameter()
}).
-type ebi_rdef() ::
    #ebi_rdef_simple{} |
    #ebi_rdef_fast{} |
    #ebi_rdef_mm{}.
-record(ebi_reaction, {
    name                :: reaction(),
    description         :: string(),
    definition          :: ebi_rdef()
}).


%%
%%  Medium definitions.
%%
-record(ebi_comp_species, {
    species             :: species(),
    concentration       :: parameter(),
    diffusion           :: parameter()
}).


-record(ebi_mdef_cnt_electrode, {

}).
-record(ebi_mdef_perforated, {

}).
-type ebi_mdef() ::
    #ebi_mdef_cnt_electrode{} |
    #ebi_mdef_perforated{}.


%%
%%  Compartment definition
%%
-record(ebi_cdef_solid_electrode, {
    el_reaction         :: reaction()
}).
-record(ebi_cdef_solution, {
    species             :: [#ebi_comp_species{}],
    nernst_thickness    :: parameter()
}).
-record(ebi_cdef_diffusive, {
    species             :: [#ebi_comp_species{}],
    reactions           :: [reaction()],
    thickness           :: parameter()
}).
-record(ebi_cdef_insulating, {
}).
-type ebi_cdef() ::
    #ebi_cdef_solid_electrode{} |
    #ebi_cdef_solution{} |
    #ebi_cdef_diffusive{} |
    #ebi_cdef_insulating{}.
-record(ebi_compartment, {
    name                :: compartment(),
    description         :: string(),
    definition          :: ebi_cdef()
}).


-record(ebi_model, {
    species = []        :: [#ebi_species{}],
    reactions = []      :: [#ebi_reaction{}],
    compartments = []   :: [#ebi_compartment{}]
}).


-endif.

