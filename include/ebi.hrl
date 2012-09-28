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
%%  @type biosensor() = #biosensor{id = atom(), description = string()}.
%%  Describes one biosensor, for which several models can be defined.
%%  I am not sure, wether will it be needed. It could be used for
%%  grouping simulations, but labels and series are also suitable for this. 
%%
-record(biosensor, {id, description}).


%%
%%  @type model() = #model{
%%      type = atom(),
%%      definition = any()
%%      }.
%%  Describes biosensor model definition. Types can be
%%      `kp1_xml' for raw XML as a binary and
%%      `kp1_parsed' for the model transformed to tuples.
%%  Contents of the `definition' depends on the type.
%%
-record(model, {
    type,
    definition
    }).


%%
%%  @type param() = #param{
%%      name = atom(),
%%      value = number()
%%      }.
%%  This type should be used for specifying simulation parameter overrides.
%%  I.e. list of such records will be passed to the solver along with the
%%  model definition when performing parameter variations.
%%
-record(param, {
    name,    % Parameter name.
    value    % Parameter value.
    }).


%%
%%  @type series() = #series{
%%      id = string(),
%%      comment = string(),
%%      model = model(),
%%      default = [param()],
%%      vectors = [[param()]],
%%      labels = [string()]
%%      }.
%%  Describes several simulations for the same biosensor model
%%  with different parameter values.
%%
-record(series, {
    id,
    comment,
    model,
    default,
    vectors,
    labels
    }).


%%
%%  @type checkpoint() = #checkpoint{
%%      step = pos_integer(),
%%      time = float(),
%%      concentrations = binary(),
%%      outputs = [output_state()],
%%      listeners = [listener_state()]
%%      }
%%  where
%%      output_state() = {FileName::string(), Position::pos_integer()}
%%      listener_state() = {Name::atom(), [listener_prop()]},
%%      listener_prop() = {Property::atom(), Value::string()}.
%%  The record describes solver state, from which the simulation could be resumed.
%%  Here<ul>
%%      <li>`step' stands for simulation step number by time,</li>
%%      <li>`time' is the simulated time,</li>
%%      <li>`concentrations' is for storing all the species concentrations in all domains, and </li>
%%      <li>`outputs' holds all the output files along with current write positions,</li>
%%      <li>`listeners' is a list of tuples describing solver listeners.</li>
%%  </ul>
-record(checkpoint, {
    step,
    time,
    concentrations,
    outputs = [],
    listeners = []
    }).


%%
%%  @type output() = #output{
%%      type = atom(),
%%      name = atom(),
%%      data = {Step::pos_integer(), Time::float(), Value::term()}
%%      }.
%%  Describes output produced by one output generator (usually a listener).
%%
-record(output, {
    type,
    name,
    data
    }).


%%
%%  @type simulation() = #simulation{
%%      id = string(),
%%      version = pos_integer(),
%%      model = model(),
%%      params = [param()],
%%      state = atom(),
%%      checkpoints = [checkpoint()],
%%      outputs = []
%%      }.
%%  Describes one numerical simulation (experiment).
%%
-record(simulation, {
    id,
    version = 1,
    model,
    params = [],
    state = undefined,
    checkpoints = [],
    outputs = []
    }).

