-module(ebi_store_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_schema_creation/1,
    test_add_model/1
]).
-include_lib("common_test/include/ct.hrl").
-include("ebi.hrl").

all() ->
    [
        test_schema_creation,
        test_add_model
    ].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:load(sasl),
    application:load(lager),
    application:load(ebi_core),
    application:load(mnesia),

    ok = application:set_env(mnesia, dir, Priv),
    ok = ebi_store:install([node()]),

    ok = application:set_env(sasl, sasl_error_logger, {file, "var/log/sasl-error.log"}),
    ok = application:set_env(sasl, errlog_type, error),
    ok = application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    ok = application:set_env(lager, error_logger_redirect, true),

    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(lager),
    ok = application:start(sasl),
    ok = application:start(gproc),
    ok = application:start(xmerl),
    ok = application:start(mnesia),
    ok = application:start(ebi_core),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(ebi_core),
    ok = application:stop(mnesia),
    ok = application:stop(xmerl),
    ok = application:stop(gproc),
    ok = application:stop(sasl),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler),
    ok = application:stop(lager),
    ok.

test_schema_creation(_Config) ->
    % Schema and tables are created in the `init_per_suite`.
    ok.


test_add_model(_Config) ->
    Model01 = #model{
        % ID and REF are skipped here.
        name = "test",
        description = "My test model",
        status = active,
        changed = erlang:now(),
        changed_by = "me",
        definition = {sbml_model},
        parameters = ["S", "P"],
        representations = [{kpxml_v1, <<"<some_xml/>">>}]
    },
    {ok, ModelId, ModelRef} = ebi_store:add_model(Model01),

    Model02 = Model01#model{
        id = ModelId,
        ref = ModelRef,
        description = "Changed description",
        changed = erlang:now()
    },
    {ok, ModelId, ModelRef} = ebi_store:add_model(Model02),

    Model03 = Model02#model{
        description = "Other",
        changed = erlang:now(),
        definition = {sbml_model, some_content}
    },
    {ok, ModelId, ModelRef3} = ebi_store:add_model(Model03),
    ModelRef /= ModelRef3,

    Model03GetLatest = Model03#model{ref = ModelRef3, representations = []},
    ct:pal("Model03GetLatest=~p~n", [Model03GetLatest]),
    {ok, Model03GetLatest} = ebi_store:get_model(ModelId),

    Model02Get = Model02#model{description = "Other", representations = []},
    ct:pal("Model02Get=~p~n", [Model02Get]),
    {ok, Model02Get} = ebi_store:get_model(ModelId, ModelRef),
    ok.



