%%% Textgroup server.
%%%
%%% Copyright (c) 2022 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(textgroup_client_sup).
-author('holger@zedat.fu-berlin.de').
-behaviour(supervisor).
-export([start_link/0,
         init/1]).

-include_lib("kernel/include/logger.hrl").
-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG_DEBUG("Starting client supervisor"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_DEBUG("Initializing client supervisor"),
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [#{id => textgroup_client,
                    start => {textgroup_client, start_link, []},
                    restart => temporary}],
    {ok, {SupFlags, ChildSpecs}}.
