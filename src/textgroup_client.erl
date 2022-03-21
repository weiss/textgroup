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

-module(textgroup_client).
-author('holger@zedat.fu-berlin.de').
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").
-define(EOL, "\r\n").
-define(WELCOME_MSG, "Welcome to Textgroup! Type 'help' for help.").
-define(GOODBYE_MSG, "Thanks for using Textgroup. See you!").
-define(HELP_MSG,
        "peers   Show the IP addresses of your current peers" ?EOL
        "stats   Show some statistic regarding this session" ?EOL
        "help    Show this help message" ?EOL
        "quit    Quit this session" ?EOL).

-record(textgroup_client_state,
        {socket :: gen_tcp:socket() | undefined,
         client :: binary() | undefined,
         n_sent = 0 :: non_neg_integer(),
         n_rcvd = 0 :: non_neg_integer()}).

-type state() :: #textgroup_client_state{}.

%% API: supervisor callback.

-spec start_link(gen_tcp:socket()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Socket) ->
    ?LOG_DEBUG("Creating client handler process"),
    gen_server:start_link(?MODULE, [Socket], []).

%% API: gen_server callbacks.

-spec init([gen_tcp:socket()]) -> {ok, state()}.
init([Socket]) ->
    process_flag(trap_exit, true),
    {ok, N} = application:get_env(tcp_queue_size),
    {ok, {LAddr, LPort}} = inet:sockname(Socket),
    {ok, {RAddr, RPort}} = inet:peername(Socket),
    Client = list_to_binary(inet:ntoa(RAddr)),
    Greeting = <<?WELCOME_MSG ?EOL
                 "Your IP address: ", Client/binary, ?EOL
                 "Peers may query your IP address." ?EOL>>,
    ok = gen_tcp:send(Socket, Greeting),
    ok = inet:setopts(Socket, [{active, N}]),
    ?LOG_DEBUG("Handling connection: ~s:~B -> ~s:~B",
               [inet:ntoa(RAddr), RPort,
                inet:ntoa(LAddr), LPort]),
    {ok, #textgroup_client_state{socket = Socket, client = Client}}.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {error, term()}, state()}.
handle_call(get_addr, From, #textgroup_client_state{client = Client} = State) ->
    ?LOG_DEBUG("Returning client address to ~p: ~s", [From, Client]),
    {reply, Client, State};
handle_call(Request, From, State) ->
    ?LOG_ERROR("Got unexpected request from ~p: ~p", [From, Request]),
    {reply, {error, badarg}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({msg, Msg}, #textgroup_client_state{socket = Socket,
                                                client = Client,
                                                n_rcvd = Rcvd} = State) ->
    ?LOG_DEBUG("Received message for ~s: ~s", [Client, Msg]),
    ok = gen_tcp:send(Socket, Msg),
    {noreply, State#textgroup_client_state{n_rcvd = Rcvd + 1}};
handle_cast(Msg, State) ->
    ?LOG_ERROR("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({tcp, Socket, <<"quit", EOL/binary>>},
            #textgroup_client_state{client = Client} = State)
  when EOL =:= <<$\n>>;
       EOL =:= <<$\r, $\n>> ->
    ?LOG_DEBUG("Got quit query from ~s", [Client]),
    Response = [<<?GOODBYE_MSG>>, EOL],
    ok = gen_tcp:send(Socket, Response),
    {stop, normal, State};
handle_info({tcp, Socket, <<"help", EOL/binary>>},
            #textgroup_client_state{client = Client} = State)
  when EOL =:= <<$\n>>;
       EOL =:= <<$\r, $\n>> ->
    ?LOG_DEBUG("Got help query from ~s", [Client]),
    Response = <<?HELP_MSG>>,
    ok = gen_tcp:send(Socket, Response),
    {noreply, State};
handle_info({tcp, Socket, <<"stats", EOL/binary>>},
            #textgroup_client_state{client = Client,
                                    n_sent = Sent,
                                    n_rcvd = Rcvd} = State)
  when EOL =:= <<$\n>>;
       EOL =:= <<$\r, $\n>> ->
    ?LOG_DEBUG("Got stats query from ~s", [Client]),
    Response = io_lib:format("Messages sent: ~B~s"
                             "Messages rcvd: ~B~s",
                             [Sent, EOL, Rcvd, EOL]),
    ok = gen_tcp:send(Socket, Response),
    {noreply, State};
handle_info({tcp, Socket, <<"peers", EOL/binary>>},
            #textgroup_client_state{client = Client} = State)
  when EOL =:= <<$\n>>;
       EOL =:= <<$\r, $\n>> ->
    ?LOG_DEBUG("Got peers query from ~s", [Client]),
    foreach_peer(fun(PID) ->
                         try gen_server:call(PID, get_addr) of
                             Addr ->
                                 Response = [Addr, EOL],
                                 ok = gen_tcp:send(Socket, Response)
                         catch exit:_ ->
                                 ok
                         end
                 end),
    {noreply, State};
handle_info({tcp, _Socket, Data},
            #textgroup_client_state{client = Client, n_sent = Sent} = State) ->
    ?LOG_DEBUG("Sending text message from ~s to peers", [Client]),
    foreach_peer(fun(PID) -> ok = gen_server:cast(PID, {msg, Data}) end),
    {noreply, State#textgroup_client_state{n_sent = Sent + 1}};
handle_info({tcp_passive, Socket},
            #textgroup_client_state{client = Client} = State) ->
    ?LOG_DEBUG("Resetting active queue size for ~s", [Client]),
    {ok, N} = application:get_env(tcp_queue_size),
    ok = inet:setopts(Socket, [{active, N}]),
    {noreply, State};
handle_info({tcp_closed, _Socket},
            #textgroup_client_state{client = Client} = State) ->
    ?LOG_DEBUG("~s closed the TCP connection", [Client]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason},
            #textgroup_client_state{client = Client} = State) ->
    ?LOG_NOTICE("Got TCP error for ~s: ~p", [Client, Reason]),
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_ERROR("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, #textgroup_client_state{socket = Socket, client = Client}) ->
    ?LOG_INFO("Closing session of ~s (~p)", [Client, Reason]),
    ok = gen_tcp:close(Socket).

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO("Got code change request"),
    {ok, State}.

%% Internal functions.

-spec foreach_peer(fun((pid()) -> ok)) -> ok.
foreach_peer(Fun) ->
    lists:foreach(fun({_, PID, _, [textgroup_client]}) when PID =:= self() ->
                          ok;
                     ({_, PID, _, [textgroup_client]}) ->
                          ok = Fun(PID)
                  end, supervisor:which_children(textgroup_client_sup)).
