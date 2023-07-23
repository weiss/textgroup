# For Developers

## Development

Textgroup development requires [Erlang/OTP][erlang] and [Rebar3][rebar3] to be
in the `$PATH`.

### Building Textgroup

    $ rebar3 compile

### Testing Textgroup

    $ rebar3 check

### Running Textgroup

    $ rebar3 shell

### Creating Textgroup Documentation

    $ rebar3 ex_doc

### Creating a Textgroup Release

    $ rebar3 release

See the [operator documentation][ops] for hints on how to deploy and run such a
release.

## Design Hints

The Textgroup service uses the [supervision tree][supervision] shown below: The
[main][textgroup_sup] supervisor starts a worker child (for integrating with
systemd) and two supervisor childs, [one][acceptor_sup] for supervising a
fixed-size pool of [five][pool_size] TCP connection acceptors, and
[another][client_sup] one for supervising dynamically created connection
handlers, one per client (there's six of them, in this example).

![Supervision tree][tree]

This is a straightforward structure, _except_ that the acceptor processes work
in a somewhat non-ideomatic way. However, don't let the
[implementation][acceptor] confuse you: Maybe just view it as a blackbox for the
moment. Once everything else seems clear, here's an explanation of what's going
on in the `textgroup_acceptor` module:

- Each acceptor process blocks in `gen_tcp:accept/2` while waiting for a new
  connection. The [problem][problem] is: While waiting, the process is
  unresponsive to [system messages][sys]. Basically, OTP processes are supposed
  to only ever wait for _Erlang_ messages, to handle those in callback
  functions, and to return to waiting for the next Erlang message. As
  [`gen_tcp`][gen_tcp] (quite [against][semantics] the usual OTP semantics)
  doesn't offer a non-blocking way to accept connections (whereas there _is_ a
  non-blocking way to receive data from the socket), the acceptor processes call
  `gen_tcp:accept/2` with a timeout, so they can check for system messages every
  few seconds. One alternative is to spawn [simple][simple] (non-OTP) processes
  just for blocking in `gen_tcp:accept/1`, and then wake a proper OTP process
  for handling the new connection, basically implementing the non-blocking
  mechanism to accept connections that `gen_tcp` doesn't provide. Another option
  would be using `prim_inet:async_accept/2`, which _does_ offer this
  functionality. However, that's not a documented interface. In the future, a
  nicer solution might become available based on the new `socket` backend, which
  provides a non-blocking [`accept/2`][socket_accept] variant.

- The `textgroup_acceptor` is built as a [special process][special]. It could
  just as well be implemented as a [generic server][gen_server] with the same
  behavior. The only reason it wasn't done this way is that most `gen_server`
  features would remain unused. Matter of taste.

- When a new connection is accepted, the acceptor asks `textgroup_client_sup` to
  spawn a new process for handling the client. An alternative would be to _not_
  split the tasks of accepting and handling connections into separate processes:
  You could spawn a pool of client handler processes that wait for new
  connections, maybe using the same workaround as the `textgroup_acceptor` to
  remain responsive. Those handlers would spawn a fresh worker immediately after
  accepting a connection, handle the connection, and then terminate. This is
  [suggested][buckets] in [Learn You Some Erlang][lyse] and [Erlang and OTP in
  Action][action], for example. It would also be consistent with the usual
  Erlang pattern to create a process for each concurrent _activity_ (processing
  a client connection from begin to end) rather than each _task_ (accepting
  connections in one process and then handling them in another). However, for
  Textgroup, it seemed preferable to have a clear separation of the fixed-size
  acceptor pool on the one hand and the client handler processes on the other:
  The advantage is a one-to-one mapping of clients and (fully responsive)
  handler processes. This allows for asking `textgroup_client_sup` for a list of
  clients and communication with them without delays. This design would also
  allow more complex applications to easily close/change the listener socket
  without disconnecting existing clients.

All that said, real-world projects will often just use an existing application
(such as [Ranch][ranch]) for accepting connections.

[erlang]: https://erlang.org
[rebar3]: https://rebar3.org
[ops]: https://weiss.github.io/textgroup/operators.html
[supervision]: https://erlang.org/doc/design_principles/des_princ.html#supervision-trees
[textgroup_sup]: https://github.com/weiss/textgroup/blob/main/src/textgroup_sup.erl
[acceptor_sup]: https://github.com/weiss/textgroup/blob/main/src/textgroup_acceptor_sup.erl
[pool_size]: https://github.com/weiss/textgroup/blob/main/config/sys.config
[client_sup]: https://github.com/weiss/textgroup/blob/main/src/textgroup_client_sup.erl
[tree]: https://raw.githubusercontent.com/weiss/textgroup/main/docs/assets/supervision.png
[acceptor]: https://github.com/weiss/textgroup/blob/main/src/textgroup_acceptor.erl
[problem]: https://erlang.org/pipermail/erlang-questions/2016-April/088847.html
[sys]: https://erlang.org/doc/man/sys.html
[gen_tcp]: https://erlang.org/doc/man/gen_tcp.html
[gen_server]: https://erlang.org/doc/design_principles/gen_server_concepts.html
[semantics]: https://erlang.org/pipermail/erlang-questions/2008-February/032912.html
[simple]: https://erlang.org/pipermail/erlang-questions/2017-August/093142.html
[socket_accept]: https://erlang.org/doc/man/socket.html#accept-2
[special]: https://erlang.org/doc/design_principles/spec_proc.html#special-processes
[gen_server]: https://erlang.org/doc/design_principles/gen_server_concepts.html
[buckets]: https://learnyousomeerlang.com/buckets-of-sockets#sockserv-revisited
[lyse]: https://learnyousomeerlang.com
[action]: https://www.manning.com/books/erlang-and-otp-in-action
[ranch]: https://ninenines.eu/docs/#ranch
