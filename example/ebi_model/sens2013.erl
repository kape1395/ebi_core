%%
%%   1. Is it wrong approach to specify, which simulations should be runned?
%%      Maybe we should query for needed results optionally with timeout?
%%
%%   2. Provide ???
%%
%%
-module(sens2013).
-behaviour(ebi_model).
-export([store_ref/1, model_ref/1, exp_to_sim/2, symbol_name/2]).
-export([run/1]).


%% =============================================================================
%%  Callbacks for `ebi_model`.
%% =============================================================================

store_ref(a = _Args) ->
    {ok, {ebi_store_fs, {"sens2013-dat"}}}}.


model_ref(a = _Args) ->
    {ok, {ebi_model_bsxml1, {file, bio_solver_xml_v1, "sens2013-cfg.bsxml1.xml"}}}.


exp_to_sim({exp, S}, a = _Args) ->
    A1 = proplists:get_value({l, 1}, S),
    A2 = proplists:get_value({l, 2}, S) + A1,
    A3 = proplists:get_value({l, 3}, S) + A2,
    A4 = proplists:get_value({l, 4}, S) + A3,
    {sim,
        [{{a, 1}, A1}, {{a, 2}, A2}, {{a, 3}, A3}, {{a, 4}, A4}] ++
        [ X || X = {{N, _}, _} <- S, N =/= l]
    }.


%% =============================================================================
%%  Simulations
%% =============================================================================


%%
%%  Common parameter values.
%%
-define(S0s, [0.49, 0.99, 1.99, 4.98, 9.9, 19.6]).  % mol/m^3
-define(M0s, [0.2, 0.05, 0.005]).                   % mol/m^3


%% -----------------------------------------------------------------------------
%%  Helper functions.
%% -----------------------------------------------------------------------------

%%
%%  Base configuration.
%%
base(a = _Args) ->
    {exp, [
        {{l, 1}, 0.002E-3},
        {{l, 2}, 0.002E-3},
        {{l, 3}, 0.010E-3},
        {{l, 4}, 0.150E-3},
        {{'D', 1}, 3.00E-10},
        {{'D', 2}, 4.20E-10},
        {{'D', 3}, 3.75E-10},
        {{'D', 4}, 6.00E-10},
        {{'E', 0}, 0.012E+0},
        {{'M', 0}, 0.050E+0},
        {{'S', 0}, 4.980E+0},
        {{k, 1}, 6.9E2},
        {{k, 2}, 4.0E4}
    ]}.

%%
%%
%%
submit_by_S0(Args, ExpSymbols) ->
    SubmitFun = fun (S0) ->
        ExpSymbolsWithS0 = ebi_model:set({'S', 0}, S0, ExpSymbols),
        {ok, Ref} = ebi:submit({?MODULE, Args}, ExpSymbolsWithS0),
        {S0, Ref}
    end,
    [ SubmitFun(S0) || S0 <- ?S0s ].

steady_by_S0(Args, Refs, Timeout) ->
    ResultFun = fun (S0, Ref) ->
        Res = ebi:result(Ref, Timeout, [
            {steady, current},
            {steady, time},
            {steady, halftime}
        ]),
        [S0 | Res]
    end,
    [ ResultFun(S0, Ref) || {S0, Ref} <- Refs ]

variate_by_S0(Args, Symbol, Values)
    ExpRefs = [
        {X, submit_by_S0(Args, ebi_model:set(Symbol, X, base(Args)))},
        || X <- Values
    ],
    [
        {X, steady_by_S0(Args, Refs, 3600000)},
        || {X, Refs} <- ExpRefs
    ].


%% -----------------------------------------------------------------------------
%%  Variations.
%% -----------------------------------------------------------------------------

-define(STD_SS_COLS, {<<"S0">>, <<"I">>, <<"T">>, <<"T05">>}).

run(graph_ss_by_D1) ->
    ok = ss_csv(
        {series, {<<"D_1">>, ?STD_SS_COLS}},
        variate_by_S0(a, {'D', 1}, [0.001, 0.002, 0.003]),
        "graph_ss_by_D1.dat"
    );

run(graph_ss_by_D2) ->
    ok = ss_csv(
        {series, {<<"D_2">>, ?STD_SS_COLS}},
        variate_by_S0(a, {'D', 2}, [0.001, 0.002, 0.003]),
        "graph_ss_by_D2.dat"
    ).


