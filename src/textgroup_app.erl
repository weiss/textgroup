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

-module(textgroup_app).
-author('holger@zedat.fu-berlin.de').
-behaviour(application).
-export([start/2,
         stop/1]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ?LOG_NOTICE("Starting Textgroup ~s on Erlang/OTP ~s (ERTS ~s)",
                [version(),
                 erlang:system_info(otp_release),
                 erlang:system_info(version)]),
    textgroup_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_NOTICE("Stopping Textgroup ~s on Erlang/OTP ~s (ERTS ~s)",
                [version(),
                 erlang:system_info(otp_release),
                 erlang:system_info(version)]),
    ok.

%% Internal functions.

-spec version() -> binary().
version() ->
    {ok, Version} = application:get_key(vsn),
    unicode:characters_to_binary(Version).
