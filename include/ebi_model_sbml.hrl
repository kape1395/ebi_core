%%
%%  SBML data model definition. This definition is based on SBML level 3 version 1 - core, release 1.
%%  http://heanet.dl.sourceforge.net/project/sbml/specifications/Level%203%20Ver.%201/sbml-level-3-version-1-core-rel-1.pdf
%%
-ifndef(ebi_model_sbml_hrl).
-define(ebi_model_sbml_hrl, ok).

-type sbml_sid() :: string().
-type sbml_sidref() :: sbml_sid().
-type sbml_unit_sid() :: sbml_sid().
-type sbml_unit_sidref() :: sbml_unit_sid().
-type sbml_name() :: string().
-type sbml_math() :: term().
-type sbml_math_lambda() :: term().
-type sbml_si_units() :: (
    ampere | farad | joule | lux | radian | volt | avogadro | gram | katal |
    metre | second | watt | becquerel | gray | kelvin | mole | siemens | weber |
    candela | henry | kilogram | newton | sievert | coulomb | hertz | litre |
    ohm | steradian | dimensionless | item | lumen | pascal | tesla
).

-record(sbml_sbase, {
    metaid          :: string(),
    sboTerm         :: string(),
    notes = []      :: [term()],
    annotation = [] :: [term()]
}).

-record(sbml_model, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    substance_units     :: sbml_unit_sidref(),
    time_units          :: sbml_unit_sidref(),
    volume_units        :: sbml_unit_sidref(),
    area_units          :: sbml_unit_sidref(),
    length_units        :: sbml_unit_sidref(),
    extent_units        :: sbml_unit_sidref(),
    conversion_factor   :: sbml_sidref(),
    list_of_function_definitions    = [],
    list_of_unit_definitions        = [],
    list_of_compartments            = [],
    list_of_species                 = [],
    list_of_parameters              = [],
    list_of_initial_assignments     = [],
    list_of_rules                   = [],
    list_of_constraints             = [],
    list_of_reactions               = [],
    list_of_events                  = []
}).
-record(sbml_function_definition, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    math                :: sbml_math_lambda()
}).

-record(sbml_unit, {
    sbase               :: #sbml_sbase{},
    kind                :: sbml_unit_sid(),
    exponent,
    scale,
    multiplier
}).
-record(sbml_unit_definition, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    list_of_units = []  :: [#sbml_unit{}]
}).
-record(sbml_compartment, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    spatial_dimensions,
    size,
    units,
    constant
}).
-record(sbml_species, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    compartment,
    initial_amount,
    initial_concentration,
    substance_units,
    has_only_substance_units,
    boundary_condition,
    constant,
    conversion_factor
}).
-record(sbml_parameter, {
    sbase               :: #sbml_sbase{},
    id                  :: sbml_sid(),
    name                :: sbml_name(),
    value,
    units,
    constant
}).
-record(sbml_initial_assignment, {
    sbase               :: #sbml_sbase{},
    symbol              :: sbml_sidref(),
    math                :: sbml_math()
}).
-record(sbml_rule, {
}).
-record(sbml_constraint, {
}).
-record(sbml_reaction, {
}).
-record(sbml_event, {
}).

-record(sbml, {
    level = 3   :: integer(),
    version = 1 :: integer(),
    namespace = 'http://www.sbml.org/sbml/level3/version1/core' :: atom(),
    model       :: #sbml_model{}
}).

-endif.