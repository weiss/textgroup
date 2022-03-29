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

-module(textgroup_acceptor).
-author('holger@zedat.fu-berlin.de').
-export([start_link/1]).
-export([init/1]).
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4]).

-include_lib("kernel/include/logger.hrl").

-record(acceptor_state,
        {parent :: pid() | undefined,
         listener :: gen_tcp:socket() | undefined}).

-type state() :: #acceptor_state{}.

%% API.

-spec start_link(gen_tcp:socket()) -> term() | {error, term()}.
start_link(Listener) ->
    ?LOG_DEBUG("Creating acceptor process"),
    State = #acceptor_state{parent = self(), listener = Listener},
    proc_lib:start_link(?MODULE, init, [State]).

-spec init(state()) -> no_return().
init(State) ->
    ?LOG_DEBUG("Initializing acceptor process"),
    proc_lib:init_ack({ok, self()}),
    loop(State).

-spec system_continue(pid(), [sys:dbg_opt()], state()) -> no_return().
system_continue(_Parent, _Debug, State) ->
    ?LOG_DEBUG("Continuing acceptor process after handling system message"),
    loop(State).

-spec system_terminate(term(), pid(), [sys:dbg_opt()], state()) -> no_return().
system_terminate(Reason, _Parent, _Debug, _State) ->
    ?LOG_DEBUG("Terminating acceptor process"),
    exit(Reason).

-spec system_code_change(state(), module(), term() | undefined, term())
      -> {ok, state()}.
system_code_change(State, _Mod, _OldVsn, _Extra) ->
    ?LOG_DEBUG("Got code change request"),
    {ok, State}.

%% Internal functions.

-spec loop(state()) -> no_return().
loop(#acceptor_state{parent = Parent, listener = Listener} = State) ->
    case gen_tcp:accept(Listener, timer:seconds(3)) of
        {ok, Socket} ->
            ok = handle_connection(Socket);
        {error, timeout} ->
            ok
    end,
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        Msg ->
            ?LOG_ERROR("Got unexpected message: ~p", [Msg]),
            loop(State)
    after 0 ->
            loop(State)
    end.

-spec handle_connection(gen_tcp:socket()) -> ok.
handle_connection(Socket) ->
    {ok, {LAddr, LPort}} = inet:sockname(Socket),
    {ok, {RAddr, RPort}} = inet:peername(Socket),
    ?LOG_INFO("Accepting connection: ~s:~B -> ~s:~B",
              [inet:ntoa(RAddr), RPort,
               inet:ntoa(LAddr), LPort]),
    {ok, Child} = supervisor:start_child(textgroup_client_sup, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Child).
