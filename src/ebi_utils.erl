%
% Copyright 2012-2013 Karolis Petrauskas
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
%% @doc Main interface of the `ebi' application.
%% @headerfile "ebi.hrl"
%%
-module(ebi_utils).
-export([sha1sum/1]).


%% =============================================================================
%%  Public API.
%% =============================================================================


-spec sha1sum(binary()) -> string().
sha1sum(Binary) when is_binary(Binary) ->
    SHA = crypto:hash(sha, Binary),
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(SHA)]).


