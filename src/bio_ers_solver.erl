%
% Copyright 2012 Karolis Petrauskas
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

%%
%% @doc This module is responsible for managing solver processes (ports).
%% The module implements FSM that is acting as a connected process for the solver port.
%% @headerfile "bio_ers_solver.hrl"
%%
-module(bio_ers_solver).
-behaviour(gen_fsm).
-export([start_link/1, start_link/2, run/1, suspend/1, cancel/1, status/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([init/2, running/2, restarting/2, suspending/2, suspended/2]).
-include("bio_ers.hrl").
-include("bio_ers_solver.hrl").


-define(DEFAULT_PORT_PROGRAM, "priv/bio_ers_solver_port").
-define(RC_UNDEFINED,          10).
-define(RC_SIMULATION_DONE,    11).
-define(RC_SIMULATION_STOPPED, 12).
-define(RC_SIMULATION_FAILED,  13).

%%
%%
%%
-record(fsm_state, {simulation, port, prog, state}).


%%
%%  @todo module doc.
%%  see doc/bio_ers_uml/bio_ers_solver.svg
%%
%%  States:
%%      init
%%      running
%%      suspending
%%      suspended
%%      restarting
%%
%%  Events:
%%      solver_run
%%      solver_suspend
%%      solver_cancel      (global)
%%      solver_status      (global)
%%      simulation_done    (port rc)
%%      simulation_failed  (port rc)
%%      simulation_stopped (port rc)
%%      simulation_error   (port rc)
%%      simulation_state   (global)
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public API.
%%


%%
%%  @doc Start the solver with the default port program.
%%  @see start_link/2.
%%
start_link(Simulation) ->
    start_link(Simulation, ?DEFAULT_PORT_PROGRAM).

%%
%%  @doc Start the solver. The solver is started in the init state, waiting
%%  for a command to run the simulation.
%%
start_link(Simulation, PortProgram) ->
    gen_fsm:start_link(?MODULE, {Simulation, PortProgram}, []).


%%
%%  @doc Run the actual simulation or resume it, if the simulation was suspended.
%%
run(FsmRef) ->
    gen_fsm:send_event(FsmRef, solver_run).

%%
%%  @doc Suspend the simulation, if it is running now.
%%
suspend(FsmRef) ->
    gen_fsm:send_event(FsmRef, solver_suspend).

%%
%%  @doc Cancel the simulation and delete all the related data.
%%  Use this in the case, when the simulation is not relevant anymore.
%%
cancel(FsmRef) ->
    gen_fsm:send_all_state_event(FsmRef, solver_cancel).

%%
%%  @doc Returns current status of the solver.
%%
status(Pid) when erlang:is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> gen_fsm:sync_send_all_state_event(Pid, solver_status);
        false -> down        % @todo get status from DB.
    end;
status(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, solver_status).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%  @doc Initialize the FSM.
%%  @todo read process state from DB if needed.
%%
init({Simulation, PortProgram}) when is_record(Simulation, simulation) ->
    process_flag(trap_exit, true),
    {ok, init, #fsm_state{
        simulation = Simulation,
        port = undefined,
        prog = PortProgram
    }}.


%%
%%  @doc The solver_cancel event can occur at any time during lifecycle
%%  of the solver.
%%
handle_event(solver_cancel, _StateName, StateData) ->
    {stop, normal, StateData#fsm_state{state = canceled}};
handle_event(simulation_state, StateName, StateData) ->
    % @todo save data.
    {next_state, StateName, StateData}.


%%
%%  @doc Synchronous query for the current status can be invoked at any time.
%%
handle_sync_event(solver_status, _From, StateName, StateData) ->
    {reply, {StateName}, StateName, StateData}.


%%
%%  @doc This function will be invoked for any other messages than normal events.
%%  The events will include:
%%      messages from the port.
%%      port exit status
%%      trap exit?
%%  Port exit statuses are forwarded to the corresponding FSM state functions.
%%  Normal exit messages ({'EXIT', ..}) from the port are ignored.
%%
handle_info({_Port, {exit_status, RC}}, StateName, StateData) ->
    Event = case RC of
        ?RC_SIMULATION_DONE    -> simulation_done;
        ?RC_SIMULATION_STOPPED -> simulation_stopped;
        ?RC_SIMULATION_FAILED  -> simulation_failed;
        ?RC_UNDEFINED          -> simulation_error;
        _                      -> simulation_error
    end,
    case StateName of
        running    -> running(Event, StateData);
        restarting -> restarting(Event, StateData);
        suspending -> suspending(Event, StateData)
    end;
handle_info({'EXIT', Port, normal}, StateName, StateData) when is_port(Port) ->
    {next_state, StateName, StateData};
handle_info(Info, StateName, StateData) ->
    io:format("# handle_info: Info=~p StateName=~p~n", [Info, StateName]),
    {next_state, StateName, StateData}.


%%
%%  @doc Termination of the FSM. Reasons can be the following:
%%      normal   - when the solver is done with its job,
%%      shutdown - when the OTP application will be going down.
%%      _        - just in case.
%%  The final states can be the following:
%%      done     - simulation was completed successfully.
%%      canceled - when the FSM was canceled by the user,
%%      failed   - when the simulation fails (unstable scheme, etc),
%%
terminate(normal, _StateName, #fsm_state{state = canceled}) ->
    ok;
terminate(normal, StateName, #fsm_state{state = failed}) ->
    error_logger:warning_msg("Simulation failed at state=~p~n", [StateName]),
    ok;
terminate(normal, StateName, #fsm_state{state = done}) ->
    error_logger:info_msg("Simulation done at state=~p~n", [StateName]),
    ok;
terminate(shutdown, _StateName, _StateData) ->
    ok;
terminate(Reason, StateName, _StateData) ->
    error_logger:error_msg(
        "Unexpected termination Reason=~p StateName=~p~n",
        [Reason, StateName]
    ),
    ok.


%%
%%  @doc Handler for a code_change. There are no plans to use it.
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  State callbacks.
%%

%%
%%  @doc Initialized solver. It can be either started or canceled.
%%
init(solver_run, StateData = #fsm_state{simulation = Simulation, prog = PortProgram}) ->
    Port = start_solver_port(Simulation, PortProgram),
    {next_state, running, StateData#fsm_state{state = running, port = Port}};
init(solver_suspend, StateData) ->
    {next_state, init, StateData}.


%%
%%  @doc Simulation is actually executed in this state. The solver
%%  can be suspended in this state.
%%
running(solver_run, StateData) ->
    {next_state, running, StateData};
running(solver_suspend, StateData = #fsm_state{port = Port}) ->
    stop_solver_port(Port),
    {next_state, suspending, StateData#fsm_state{state = suspended, port = undefined}};
running(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
running(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
running(simulation_stopped, StateData) ->
    % @todo Restart solver.
    {next_state, running, StateData}.

%%
%%  @doc The state restarting is considered for the cases, when
%%  someone is sending solver_run event while solver is in the
%%  suspending state.
%%
restarting(solver_run, StateData) ->
    {next_state, restarting, StateData};
restarting(solver_suspend, StateData) ->
    {next_state, suspending, StateData#fsm_state{state = suspended}};
restarting(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
restarting(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
restarting(simulation_stopped, StateData) ->
    % @todo Restart solver.
    {next_state, running, StateData}.


%%
%%  @doc The solver is requested to be suspended. Now we are waiting
%%  the solver to notify, that the simulation is stopped gracefully.
%%
suspending(solver_run, StateData) ->
    {next_state, restarting, StateData};
suspending(solver_suspend, StateData) ->
    {next_state, suspending, StateData};
suspending(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
suspending(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
suspending(simulation_stopped, StateData) ->
    {next_state, suspended, StateData#fsm_state{port = undefined}}.



%%
%%  @doc The solver is already suspended completely. One can resume it.
%%
suspended(solver_run, StateData = #fsm_state{simulation = Simulation, prog = PortProgram}) ->
    Port = start_solver_port(Simulation, PortProgram),
    {next_state, running, StateData#fsm_state{port = Port}};
suspended(solver_suspend, StateData) ->
    {next_state, suspended, StateData}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Creates and configures the solver port.
%%
start_solver_port(Simulation, PortProgram) when is_record(Simulation, simulation) ->
    #simulation{id = Id, model = Model, params = Params} = Simulation,
    Port = erlang:open_port(
       {spawn_executable, PortProgram},
       [{packet, 2}, use_stdio, exit_status, binary]
    ),
    erlang:port_command(Port, erlang:term_to_binary(
        #configure_port{
            self = self(),
            id = Id,
            model = Model,
            params = Params,
            checkpoint = undefined % @todo Implement support for checkpoints.
        },
        [{minor_version, 1}]
    )),
    Port.

%%
%%  @doc Sents stop command to the solver. The solver should terminate gracefully 
%%  and send corresponding messages to this process. Those messages should be
%%  handled by handle_info/3.
%%
stop_solver_port(Port) ->
    erlang:port_command(Port, erlang:term_to_binary(
        #stop_port{self = self()},
        [{minor_version, 1}]
    )).

