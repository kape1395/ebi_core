-module(test_proc).
-export([fit_effective_diff_coef/3]).
-export([func_seq/2, func_seq/3]).
-record(params, {dif1, dif2, dif3, dif4}).
-record(dexp, {d, j, t}).

expD(#dexp{d = D}) -> D.
expJ(#dexp{j = J}) -> J.
expT(#dexp{t = T}) -> T.

%%
%%  Model :: #model{}                 -- Biosensor 1D model
%%  Params :: #params{}               -- Basic model parameters
%%  Target :: {CurrentDensity, Time}  -- Response of the 2D model
%%
fit_effective_diff_coef(Model, #params{dif2 = D2, dif3 = D3} = Params, Target) ->
    FitParam = {#params.dif3, FitMin = 1.0E-20, FitMax = max(D2, D3)},
    PreliminaryVarFun = fun
        (start) -> FitMax;
        (X) when X >= FitMin  -> X / 2;
        (X) when X < FitMin -> undefined
    end,
    ExperimentResFun = fun
        (_) -> #dexp{d = 0, j = 0, t = 0}
    end,
    PreliminaryExperiments = preliminary_variation(
        Model, Params, FitParam, Target,
        ExperimentResFun, PreliminaryVarFun, 30
    ).


%%
%% Step 2. Preliminary variation.
%%
preliminary_variation(Model, Params, FitParam, Target, ExperimentResFun, PreliminaryVarFun, MaxExperiments) ->
    Sequence = func_seq(PreliminaryVarFun, MaxExperiments),
    NextStep = fun (SimResults) ->
        Experiments = ExperimentResFun(lists:zip(Sequence, SimResults)),
        find_ranges(Model, Params, FitParam, Target, ExperimentResFun, Experiments)
    end,
    simulate(make_series(Model, FitParam, Sequence), NextStep).


find_ranges(Model, Params, FitParam, Target, ExperimentResFun, Experiments) ->
    ok
    .


%%
%%  Step 3. Find the candidate intervals.
%%
step_3(Experiments, Target) ->
    Intervals = containing_intervals(Experiments, Target, fun expD/1),
    case Intervals of
        [] -> step_4(Experiments, Target);
        _  -> step_6(Experiments, Target, Intervals)
    end.

%%
%%  Step 4. Find the nearest one.
%%  TODO: Fix.
step_4(Experiments, Target) ->
    Epsilon = 1.0e-5,
    Nearest = nearest(Experiments, Target, fun expJ/1),
    #dexp{j = NearestJ} = Nearest,
    case abs(NearestJ - Target) < Epsilon of
        true  -> {Nearest, Experiments};
        false -> {undefined, Experiments}
    end.

%%
%%
%%
step_5() -> ok.

%%
%%  Step 6. Method of sectands for each interval.
%%  Result is the solution with the best fit by the halftime.
%%
step_6(Experiments, Target, Intervals) ->
    Solutions = [ chords(I, Target, fun expD/1, fun expJ/1, 1.0e-5) || I <- Intervals ],
    SolutionsByDistT = [ {abs(expT(S) - expT(Target)), S} || {S, _} <- Solutions ],
    {_, BestByTimeSolution} = lists:min(SolutionsByDistT),
    AllExperiments = [ Experiments, [ E || {_, E} <- Solutions ] ],
    {BestByTimeSolution, lists:sort(fun expD/1, lists:flaten(AllExperiments))}.


chords(_Interval, _Target, _ParamToChange, _ParamToMinimize, _Error) -> {solution, [experiment]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simulate(_Series, _Continuation) -> ok.
make_series(_Model, _Param, _Range) -> {series}.


%%
%%  Find an Element in List, who's F(Element) is nearest to Target.
%%
nearest(List, Target, F) ->
    nearest(List, Target, F, undefined).

nearest([], _Target, _F, Nearest) ->
    Nearest;
nearest([Elem | Tail], Target, F, undefined) ->
    nearest(Tail, Target, F, Elem);
nearest([Elem | Tail], Target, F, Nearest) ->
    ElemVal = F(Elem),
    NearestVal = F(Nearest),
    case abs(Target - ElemVal) < abs(Target - NearestVal) of
       true  -> nearest(Tail, Target, F, Elem);
       false -> nearest(Tail, Target, F, Nearest)
    end.

%%
%%
%%
containing_intervals(List, Target, F) ->
    lists:reverse(containing_intervals(List, Target, F, [])).

containing_intervals([A, B | Tail], Target, F, Intervals) ->
    AVal = F(A),
    BVal = F(B),
    if
        AVal =< Target, Target =< BVal;
        AVal >= Target, Target >= BVal -> containing_intervals([B | Tail], Target, F, [{A, B}, Intervals]);
        true                           -> containing_intervals([B | Tail], Target, F, Intervals)
    end;
containing_intervals(_List, _Target, _F, Intervals) ->
    Intervals.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%  Step 3.a. Finds intervals with X inside.
%%
%intervals([A, B | Tail], X) when A =< X, B >= X ->
%    [ {A, B} | intervals([B | Tail], X) ];
%intervals([A | Tail], X) -> intervals(Tail, Target);
%intervals([], _X) -> [].

    

%fit_param_by_variation(Model, {Parameter, Min, Max}, ResponseTarget, Experiments, {D, J, T}) ->
%    simulate(
%        make_series(Model, Parameter, exp_seq(ParamMax, ParamMin, 0.5, 10)),
%        fun (Experiments) -> fit_param_in_seq(Experiments, Model, Parameter, ResponseTarget) end
%    ).

%fit_param_in_seq(Experiments, Model, Parameter, Target) ->
%    Interval = find_closest(Experiments, Target),
%    Error = 0.001,
%    % min size.
%    case point_in_interval(Interval, Target, Error) of
%        'start' -> {Result, _} = Interval, Result;
%        'end'   -> {_, Result} = Interval, Result;
%        'between' -> fit_parameter(Model, Parameter, Interval, Target);
%        'outside' -> no_solution
%    end
%    .



linear_seq(_From, _To, _Increment, _MaxCount) ->
    [].
exp_seq(_From, _To, _Exponent, _MaxCount) ->
    [].

%%
%%  Generates sequence from function:
%%      Function(start | Number) -> NextNumber | undefined.
%%  Start of sequence is generated by passing 'start' to the function.
%%  End of the sequence is detected by getting 'undefined' from the function.
%%
func_seq(Function, MaxCount) ->
    func_seq(Function, start, MaxCount).
func_seq(Function, From, MaxCount) ->
    lists:reverse(func_seq_internal(Function, Function(From), MaxCount, [])).

func_seq_internal(_Function, From, MaxCount, Seq) when MaxCount == 0; From == undefined ->
    Seq;
func_seq_internal(Function, From, MaxCount, Seq) when MaxCount > 0 ->
    func_seq_internal(Function, Function(From), MaxCount - 1, [From | Seq]).
    

%find_closest_interval([A, B, | Tail], Target, Function) ->
%    find_closest_interval([B | Tail]);
%find_closest_interval(List, Target, Function) ->
%    undefined.
%    
%
%find_closest_interval(List, Target, Function, Closest) ->

