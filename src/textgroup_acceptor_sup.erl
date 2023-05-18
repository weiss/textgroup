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

-module(textgroup_acceptor_sup).
-behaviour(supervisor).
-export([start_link/0,
         init/1]).

-include_lib("kernel/include/logger.hrl").
-define(SERVER, ?MODULE).
-define(SEND_TIMEOUT, timer:seconds(15)).

%% API.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG_DEBUG("Starting acceptor supervisor"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_DEBUG("Initializing acceptor supervisor"),
    {ok, Port} = application:get_env(port),
    {ok, PoolSize} = application:get_env(pool_size),
    Listener = start_listener(Port),
    SupFlags = #{},
    ChildSpecs =
        lists:map(
          fun(N) ->
                  #{id => {textgroup_acceptor, N},
                    start => {textgroup_acceptor, start_link, [Listener]}}
          end, lists:seq(1, PoolSize)),
    {ok, {SupFlags, ChildSpecs}}.

%% Internal functions.

-spec start_listener(inet:port_number()) -> inet:socket().
start_listener(Port) ->
    ?LOG_DEBUG("Starting listener on port ~B", [Port]),
    {ok, Listener} = gen_tcp:listen(Port, [binary,
                                           {reuseaddr, true},
                                           {nodelay, true},
                                           {active, false},
                                           {packet, line},
                                           {send_timeout, ?SEND_TIMEOUT},
                                           {send_timeout_close, true}]),
    Listener.
