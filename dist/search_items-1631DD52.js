searchNodes=[{"type":"module","title":"textgroup_acceptor","doc":"","ref":"textgroup_acceptor.html"},{"type":"function","title":"textgroup_acceptor.init/1","doc":"","ref":"textgroup_acceptor.html#init/1"},{"type":"function","title":"textgroup_acceptor.loop/1","doc":"","ref":"textgroup_acceptor.html#loop/1"},{"type":"function","title":"textgroup_acceptor.start_link/1","doc":"","ref":"textgroup_acceptor.html#start_link/1"},{"type":"function","title":"textgroup_acceptor.system_code_change/4","doc":"","ref":"textgroup_acceptor.html#system_code_change/4"},{"type":"function","title":"textgroup_acceptor.system_continue/3","doc":"","ref":"textgroup_acceptor.html#system_continue/3"},{"type":"function","title":"textgroup_acceptor.system_terminate/4","doc":"","ref":"textgroup_acceptor.html#system_terminate/4"},{"type":"opaque","title":"textgroup_acceptor.state/0","doc":"","ref":"textgroup_acceptor.html#t:state/0"},{"type":"module","title":"textgroup_acceptor_sup","doc":"","ref":"textgroup_acceptor_sup.html"},{"type":"function","title":"textgroup_acceptor_sup.init/1","doc":"","ref":"textgroup_acceptor_sup.html#init/1"},{"type":"function","title":"textgroup_acceptor_sup.start_link/0","doc":"","ref":"textgroup_acceptor_sup.html#start_link/0"},{"type":"module","title":"textgroup_app","doc":"","ref":"textgroup_app.html"},{"type":"function","title":"textgroup_app.config_change/3","doc":"","ref":"textgroup_app.html#config_change/3"},{"type":"function","title":"textgroup_app.prep_stop/1","doc":"","ref":"textgroup_app.html#prep_stop/1"},{"type":"function","title":"textgroup_app.start/2","doc":"","ref":"textgroup_app.html#start/2"},{"type":"function","title":"textgroup_app.stop/1","doc":"","ref":"textgroup_app.html#stop/1"},{"type":"module","title":"textgroup_client","doc":"","ref":"textgroup_client.html"},{"type":"function","title":"textgroup_client.code_change/3","doc":"","ref":"textgroup_client.html#code_change/3"},{"type":"function","title":"textgroup_client.get_address/1","doc":"","ref":"textgroup_client.html#get_address/1"},{"type":"function","title":"textgroup_client.handle_call/3","doc":"","ref":"textgroup_client.html#handle_call/3"},{"type":"function","title":"textgroup_client.handle_cast/2","doc":"","ref":"textgroup_client.html#handle_cast/2"},{"type":"function","title":"textgroup_client.handle_info/2","doc":"","ref":"textgroup_client.html#handle_info/2"},{"type":"function","title":"textgroup_client.init/1","doc":"","ref":"textgroup_client.html#init/1"},{"type":"function","title":"textgroup_client.send/2","doc":"","ref":"textgroup_client.html#send/2"},{"type":"function","title":"textgroup_client.start/1","doc":"","ref":"textgroup_client.html#start/1"},{"type":"function","title":"textgroup_client.start_link/1","doc":"","ref":"textgroup_client.html#start_link/1"},{"type":"function","title":"textgroup_client.terminate/2","doc":"","ref":"textgroup_client.html#terminate/2"},{"type":"opaque","title":"textgroup_client.state/0","doc":"","ref":"textgroup_client.html#t:state/0"},{"type":"module","title":"textgroup_client_sup","doc":"","ref":"textgroup_client_sup.html"},{"type":"function","title":"textgroup_client_sup.init/1","doc":"","ref":"textgroup_client_sup.html#init/1"},{"type":"function","title":"textgroup_client_sup.start_link/0","doc":"","ref":"textgroup_client_sup.html#start_link/0"},{"type":"module","title":"textgroup_sup","doc":"","ref":"textgroup_sup.html"},{"type":"function","title":"textgroup_sup.init/1","doc":"","ref":"textgroup_sup.html#init/1"},{"type":"function","title":"textgroup_sup.start_link/0","doc":"","ref":"textgroup_sup.html#start_link/0"},{"type":"module","title":"textgroup_systemd","doc":"","ref":"textgroup_systemd.html"},{"type":"function","title":"textgroup_systemd.code_change/3","doc":"","ref":"textgroup_systemd.html#code_change/3"},{"type":"function","title":"textgroup_systemd.handle_call/3","doc":"","ref":"textgroup_systemd.html#handle_call/3"},{"type":"function","title":"textgroup_systemd.handle_cast/2","doc":"","ref":"textgroup_systemd.html#handle_cast/2"},{"type":"function","title":"textgroup_systemd.handle_info/2","doc":"","ref":"textgroup_systemd.html#handle_info/2"},{"type":"function","title":"textgroup_systemd.init/1","doc":"","ref":"textgroup_systemd.html#init/1"},{"type":"function","title":"textgroup_systemd.ready/0","doc":"","ref":"textgroup_systemd.html#ready/0"},{"type":"function","title":"textgroup_systemd.reloading/0","doc":"","ref":"textgroup_systemd.html#reloading/0"},{"type":"function","title":"textgroup_systemd.start_link/0","doc":"","ref":"textgroup_systemd.html#start_link/0"},{"type":"function","title":"textgroup_systemd.stopping/0","doc":"","ref":"textgroup_systemd.html#stopping/0"},{"type":"function","title":"textgroup_systemd.terminate/2","doc":"","ref":"textgroup_systemd.html#terminate/2"},{"type":"opaque","title":"textgroup_systemd.state/0","doc":"","ref":"textgroup_systemd.html#t:state/0"},{"type":"opaque","title":"textgroup_systemd.watchdog_timeout/0","doc":"","ref":"textgroup_systemd.html#t:watchdog_timeout/0"},{"type":"extras","title":"Overview","doc":"Textgroup is a groupchat service usable with TCP clients such as Telnet or Netcat. The purpose of this project is to serve as an example application for Erlang/OTP newcomers.","ref":"readme.html"},{"type":"extras","title":"Overview - Requirements","doc":"Erlang/OTP (22 or newer). Rebar3 (3.16.0 or newer).","ref":"readme.html#requirements"},{"type":"extras","title":"Overview - Quick Test","doc":"The Textgroup service can be compiled and started with an Erlang shell by running the following command: $ rebar3 shell The service is stopped by calling q(). within that shell.","ref":"readme.html#quick-test"},{"type":"extras","title":"Overview - Continuous Integration","doc":"The results of continuous integration tests can be viewed on GitHub , including test suite [logs][ct_logs] and [coverage][coverage] analysis.","ref":"readme.html#continuous-integration"},{"type":"extras","title":"Overview - Documentation","doc":"There's documentation for users , operators , and developers on the Textgroup web site .","ref":"readme.html#documentation"},{"type":"extras","title":"For Users","doc":"Textgroup can be used with TCP clients such as Telnet or Netcat. For example: $ telnet localhost 1111 Once connected, the server understands the following commands:","ref":"users.html"},{"type":"extras","title":"For Users - peers","doc":"Show the IP addresses of your current peers.","ref":"users.html#peers"},{"type":"extras","title":"For Users - stats","doc":"Show some statistic regarding the current session.","ref":"users.html#stats"},{"type":"extras","title":"For Users - help","doc":"Show a help message.","ref":"users.html#help"},{"type":"extras","title":"For Users - quit","doc":"Quit the current session. Any other messages are sent to all connected peers.","ref":"users.html#quit"},{"type":"extras","title":"For Operators","doc":"","ref":"operators.html"},{"type":"extras","title":"For Operators - Building Textgroup","doc":"The Textgroup source code can be retrieved and built using the following commands. This requires Erlang/OTP and Rebar3 to be in the $PATH . $ curl -L https://github.com/weiss/textgroup/archive/main.tar.gz | tar -C /tmp -xzf - $ cd /tmp/textgroup-main $ rebar3 as prod tar","ref":"operators.html#building-textgroup"},{"type":"extras","title":"For Operators - Deploying Textgroup","doc":"The self-contained release archive built in the previous step includes the required parts of Erlang/OTP and can be extracted to an arbitrary location. For a persistent installation, the administrator might want to create a dedicated _textgroup user and extract the release archive into that user's $HOME directory: $ sudo useradd -r -m -d /opt/textgroup _textgroup $ sudo tar -C /opt/textgroup -xzf /tmp/textgroup-main/_build/prod/rel/textgroup/textgroup-0.1.0.tar.gz $ sudo chown -R -h _textgroup:_textgroup /opt/textgroup A systemd service unit could be installed and enabled like this: $ sudo cp /opt/textgroup/etc/systemd/system/textgroup.service /etc/systemd/system $ sudo systemctl daemon-reload $ sudo systemctl --now enable textgroup","ref":"operators.html#deploying-textgroup"},{"type":"extras","title":"For Operators - Configuring Textgroup","doc":"The /opt/textgroup/releases/0.1.0/sys.config file may be edited in order to adjust configuration settings such as the TCP port number. As an alternative, those settings can be overridden on the command line in the systemd unit. For example, run sudo systemctl edit textgroup and enter: [ Service ] ExecStart = / opt / textgroup / bin / textgroup foreground - textgroup port 1111","ref":"operators.html#configuring-textgroup"},{"type":"extras","title":"For Operators - Controlling Textgroup","doc":"The Textgroup service can be controlled using the textgroup command. The caller must have the same .erlang.cookie file (with correct permissions) in their $HOME directory as the user running Textgroup. For a list of available commands, see: $ /opt/textgroup/bin/textgroup help The log output can be viewed with: $ sudo journalctl -u textgroup","ref":"operators.html#controlling-textgroup"},{"type":"extras","title":"For Operators - Advanced: Upgrading Textgroup","doc":"To create a new release that can be used to hot-upgrade the old one: $ cd /tmp/textgroup-main $ rebar3 as prod release $ editor src/*.erl $ sed -i s/0.1.0/0.2.0/ rebar.config src/textgroup.app.src $ rebar3 as prod release $ rebar3 as prod appup generate $ rebar3 as prod relup -n textgroup -v 0.2.0 $ rebar3 as prod tar The new release archive must then be copied into place (run this command and the following ones as the _textgroup user): $ cp /tmp/textgroup-main/_build/prod/rel/textgroup/textgroup-0.2.0.tar.gz /opt/textgroup/releases Finally, the actual upgrade of the running service is performed like this: $ /opt/textgroup/bin/textgroup upgrade 0.2.0 If the new release doesn't work as expected, a downgrade to the old one can be performed: $ /opt/textgroup/bin/textgroup downgrade 0.2.0 The unused release can then be removed. E.g., to uninstall the new one after downgrading: $ /opt/textgroup/bin/textgroup uninstall 0.2.0 A few caveats: If data structures (such as the client_state record) were modified, converting them during the upgrade might require additional handling . Complex applications/dependencies may well need explicit support for hot release upgrades, such as custom appup files. Also, if a different Erlang/OTP version is used to build a new release, the upgrade process involves a restart of the emulator . For this to work, the heart functionality must be enabled.","ref":"operators.html#advanced-upgrading-textgroup"},{"type":"extras","title":"For Developers","doc":"","ref":"developers.html"},{"type":"extras","title":"For Developers - Development","doc":"Textgroup development requires Erlang/OTP and Rebar3 to be in the $PATH . Building Textgroup $ rebar3 compile Testing Textgroup $ rebar3 check Running Textgroup $ rebar3 shell Creating Textgroup Documentation $ rebar3 ex_doc Creating a Textgroup Release $ rebar3 release See the operator documentation for hints on how to deploy and run such a release.","ref":"developers.html#development"},{"type":"extras","title":"For Developers - Design Hints","doc":"The Textgroup service uses the supervision tree shown below: The main supervisor starts a worker child (for integrating with systemd) and two supervisor childs, one for supervising a fixed-size pool of five TCP connection acceptors, and another one for supervising dynamically created connection handlers, one per client (there's six of them, in this example). This is a straightforward structure, except that the acceptor processes work in a somewhat non-ideomatic way. However, don't let the implementation confuse you: Maybe just view it as a blackbox for the moment. Once everything else seems clear, here's an explanation of what's going on in the textgroup_acceptor module: Each acceptor process blocks in gen_tcp:accept/2 while waiting for a new connection. The problem is: While waiting, the process is unresponsive to system messages . Basically, OTP processes are supposed to only ever wait for Erlang messages, to handle those in callback functions, and to return to waiting for the next Erlang message. As [ gen_tcp ][gen tcp] (quite against the usual OTP semantics) doesn't offer a non-blocking way to accept connections (whereas there _is a non-blocking way to receive data from the socket), the acceptor processes call gen_tcp:accept/2 with a timeout, so they can check for system messages every few seconds. One alternative is to spawn simple (non-OTP) processes just for blocking in gen_tcp:accept/1 , and then wake a proper OTP process for handling the new connection, basically implementing the non-blocking mechanism to accept connections that gen_tcp doesn't provide. Another option would be using prim_inet:async_accept/2 , which does offer this functionality. However, that's not a documented interface. In the future, a nicer solution might become available based on the new socket backend, which provides a non-blocking accept/2 variant. The textgroup_acceptor is built as a special process . It could just as well be implemented as a generic server with the same behavior. The only reason it wasn't done this way is that most gen_server features would remain unused. Matter of taste. When a new connection is accepted, the acceptor asks textgroup_client_sup to spawn a new process for handling the client. An alternative would be to not split the tasks of accepting and handling connections into separate processes: You could spawn a pool of client handler processes that wait for new connections, maybe using the same workaround as the textgroup_acceptor to remain responsive. Those handlers would spawn a fresh worker immediately after accepting a connection, handle the connection, and then terminate. This is suggested in Learn You Some Erlang and Erlang and OTP in Action , for example. It would also be consistent with the usual Erlang pattern to create a process for each concurrent activity (processing a client connection from begin to end) rather than each task (accepting connections in one process and then handling them in another). However, for Textgroup, it seemed preferable to have a clear separation of the fixed-size acceptor pool on the one hand and the client handler processes on the other: The advantage is a one-to-one mapping of clients and (fully responsive) handler processes. This allows for asking textgroup_client_sup for a list of clients and communication with them without delays. This design would also allow more complex applications to easily close/change the listener socket without disconnecting existing clients. All that said, real-world projects will often just use an existing application (such as Ranch ) for accepting connections.","ref":"developers.html#design-hints"}]