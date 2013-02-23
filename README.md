ebi
===

Erlang biosensor simulation coordinator.



Main concepts
=============


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



Design decisions
================


  * Use native data model for describing biosensors inside of this program.
    The decision has made in order to have more convenient way to represent
    biosensor structure instead of trying to follow some standard.

    SBML is the main candidate to be used as an internal data model.
    It was decided not to use it as an internal data model mainly
    because of use of MathML and lack of geometry support.

    Another reason to avoid use of some standard as an internal data model is that the standards
    are evolving and sometimes new standards replacing previous ones. Having such a model as a
    basis for all the internal communication, makes the software highly coupled with that standard.
    This makes evolution of the software more complicated, also introduces problems with supporting
    other emerging standards or new versions of existing ones mainly because the internal data model
    is used in the all the modules of the software.

    A support for SBML and other widely used standards will be provided via import/export functionality.


Developer notes
===============

To run the application interactively:

    mkdir -p temp/data/mnesia
    env ERL_LIBS=deps erl -pa ebin

and then in the Erlang shell:

    ebi_test_utils:configure("./temp").
    ebi_test_utils:install_db().        % For the first startup only.
    ebi_test_utils:start_core().
    init:stop().

