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

-module(textgroup_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-type info() :: ct_suite:ct_info().
-type config() :: ct_suite:ct_config().
-type test_def() :: ct_suite:ct_test_def().
-type test_name() :: ct_suite:ct_testname().
-type group_def() :: ct_suite:ct_group_def().
-type group_name() :: ct_suite:ct_groupname().

%% API.

-spec suite() -> [info()].
suite() ->
    [{require, {host, [addr, port]}},
     {timetrap, {seconds, 30}}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Host = ct:get_config(host),
    Addr = proplists:get_value(addr, Host),
    Port = proplists:get_value(port, Host),
    [{addr, Addr}, {port, Port} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_GroupName, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> ok.
end_per_group(_GroupName, _Config) ->
    ok.

-spec init_per_testcase(test_name(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.

-spec end_per_testcase(test_name(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
    ok.

-spec groups() -> [group_def()].
groups() ->
    [].

-spec all() -> [test_def()] | {skip, term()}.
all() ->
    [start_server,
     peers,
     stats,
     help,
     get_state,
     config_change,
     stop_server].

%% Test cases.

-spec start_server(config()) -> any().
start_server(Config) ->
    ct:pal("Starting Textgroup server"),
    Port = ?config(port, Config),
    Opts = [{persistent, true}],
    ok = application:set_env(textgroup, port, Port, Opts),
    ok = application:set_env(textgroup, tcp_queue_size, 2, Opts),
    {ok, _Apps} = application:ensure_all_started(textgroup).

-spec peers(config()) -> any().
peers(Config) ->
    ok = query(Config, <<"peers">>, <<"127.0.0.1">>).

-spec stats(config()) -> any().
stats(Config) ->
    ok = query(Config, <<"stats">>, <<"Messages rcvd: 1">>).

-spec help(config()) -> any().
help(Config) ->
    ok = query(Config, <<"help">>, <<"quit">>).

-spec get_state(config()) -> any().
get_state(_Config) ->
    {ok, Size} = application:get_env(textgroup, pool_size),
    ct:pal("Requesting state of the ~B acceptor processes", [Size]),
    ok = lists:foreach(fun({_, PID, _, [textgroup_acceptor]}) ->
                               {acceptor_state, _, _} = sys:get_state(PID)
                       end, supervisor:which_children(textgroup_acceptor_sup)).

-spec config_change(config()) -> any().
config_change(_Config) ->
    Opts = [{persistent, true}],
    Size = 10,
    ok = application:set_env(textgroup, pool_size, Size, Opts),
    ok = textgroup_app:config_change([{pool_size, Size}], [], []).

-spec stop_server(config()) -> any().
stop_server(_Config) ->
    ct:pal("Stopping Textgroup server"),
    ok = application:stop(textgroup).

%% Internal functions.

-spec connect(config()) -> gen_tcp:socket().
connect(Config) ->
    Addr = ?config(addr, Config),
    Port = ?config(port, Config),
    {ok, Socket} = gen_tcp:connect(Addr, Port,
                                   [binary,
                                    {nodelay, true},
                                    {active, false},
                                    {packet, line}],
                                   timer:seconds(5)),
    Socket.

-spec query(config(), binary(), binary()) -> ok.
query(Config, Query, Expected) ->
    ct:pal("Opening first Textgroup session"),
    Socket1 = connect(Config),
    ct:pal("Opening second Textgroup session"),
    Socket2 = connect(Config),
    ok = recv_until(Socket1, <<"Peers may query your IP address">>),
    ok = recv_until(Socket2, <<"Peers may query your IP address">>),
    ct:pal("Sending messages over both sessions"),
    ok = gen_tcp:send(Socket1, [<<"foo">>, $\n]),
    ok = gen_tcp:send(Socket2, [<<"bar">>, $\n]),
    ct:pal("Receiving messages over both sessions"),
    ok = recv_until(Socket1, <<"bar">>),
    ok = recv_until(Socket2, <<"foo">>),
    ct:pal("Querying ~s from Textgroup server", [Query]),
    ok = gen_tcp:send(Socket1, [Query, $\n]),
    ok = recv_until(Socket1, Expected),
    ct:pal("Closing first Textgroup session"),
    ok = gen_tcp:send(Socket1, <<"quit", $\n>>),
    ok = recv_until(Socket1, <<"Thanks for using Textgroup">>),
    ok = gen_tcp:close(Socket1),
    ct:pal("Closing second Textgroup session"),
    ok = gen_tcp:send(Socket2, <<"quit", $\n>>),
    ok = recv_until(Socket2, <<"Thanks for using Textgroup">>),
    ok = gen_tcp:close(Socket2).

-spec recv_until(gen_tcp:socket(), binary()) -> ok.
recv_until(Socket, Expected) ->
    ct:pal("Expecting response: ~s", [Expected]),
    {ok, Data} = gen_tcp:recv(Socket, 0, timer:seconds(5)),
    Line = string:trim(Data),
    case string:prefix(Line, Expected) of
        nomatch ->
            ct:pal("Didn't get expected response (yet): ~s", [Line]),
            recv_until(Socket, Expected);
        _Rest ->
            ct:pal("Got expected response: ~s", [Line]),
            ok
    end.
