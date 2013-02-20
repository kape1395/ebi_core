ebi
===

Erlang biosensor simulation coordinator.



Entities
========


Biosensor
:   Not modelled directly. Keywords are used for grouping models.

Model
:   ...

Model Definition
:   ...

Model Representation
:   ...

Model Source
:   Imported document (biosensor.xml, SBML, etc). It has a link to
    the corresponding model definition (the native form).

Derived Model
:   ... aka one-dimensional, dimensionless, etc..

Parameter Mapping / Model Transformations:
:   ...
    NewModel = f(Params)
    Symbol mappings?
    Standard mappings: dimless, 1d, ...?
    Custom transformations: e.g. remove a layer and update boundary conditions accordingly.

Simulation
:   One simulation, executed using particular model with fixed parameter
    values.

Simulation Series
:   Separate case of simulation procedure. It is usually based on input
    parameters only, like: {variable, from, to, step, (linear|exponential)}.

Simulation Procedure
:   Asynchronous procedure generating new simulations based on input parameters
    and results of other simulations.

Investigation
:   See keywords.

