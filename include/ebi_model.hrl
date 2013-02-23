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

-record(ebi_species, {
    name                :: species(),
    description         :: string()
}).

-record(ebi_rdef_simple, {
    reagents = []       :: [{species(), number()}],
    products = []       :: [{species(), number()}],
    rateconst           :: parameter()
}).
-record(ebi_rdef_mm, {
    substrate           :: species(),
    product             :: species(),
    vmax                :: parameter(),
    km                  :: parameter()
}).
-record(ebi_reaction, {
    name                :: string(),
    definition          :: #ebi_rdef_simple{} | #ebi_rdef_mm{}
}).

-record(ebi_model, {
    species = []        :: [#ebi_species{}],
    reactions = []      :: [#ebi_reaction{}],
    compartments = []   :: []
}).


-endif.

