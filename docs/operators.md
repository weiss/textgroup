# For Operators

## Building Textgroup

The Textgroup source code can be retrieved and built using the following
commands. This requires [Erlang/OTP][erlang] and [Rebar3][rebar3] to be
in the `$PATH`.

```shell
$ curl -L https://github.com/weiss/textgroup/archive/main.tar.gz | tar -C /tmp -xzf -
$ cd /tmp/textgroup-main
$ rebar3 as prod tar
```

## Deploying Textgroup

The self-contained [release][release] archive built in the previous step
includes the required parts of Erlang/OTP and can be extracted to an arbitrary
location. For a persistent installation, the administrator might want to create
a dedicated `_textgroup` user and extract the release archive into that user's
`$HOME` directory:

```shell
$ sudo useradd -m -d /opt/textgroup _textgroup
$ sudo tar -C /opt/textgroup -xzf /tmp/textgroup-main/_build/prod/rel/textgroup/textgroup-0.1.0.tar.gz
$ sudo chown -R -h _textgroup:_textgroup /opt/textgroup
```

A systemd service unit could be installed and enabled like this:

```shell
$ sudo cp /opt/textgroup/etc/systemd/system/textgroup.service /etc/systemd/system
$ sudo systemctl daemon-reload
$ sudo systemctl --now enable textgroup
```

## Configuring Textgroup

The `/opt/textgroup/releases/0.1.0/sys.config` file may be edited in order to
adjust configuration settings such as the TCP port number. As an alternative,
those settings can be overridden on the command line in the systemd unit. For
example, run `sudo systemctl edit textgroup` and enter:

```ini
[Service]
ExecStart=/opt/textgroup/bin/textgroup foreground -textgroup port 1111
```

## Controlling Textgroup

The Textgroup service can be controlled using the `textgroup` command. The
caller must have the same `.erlang.cookie` file (with correct permissions) in
their `$HOME` directory as the user running Textgroup. For a list of available
commands, see:

```shell
$ /opt/textgroup/bin/textgroup help
```

The log output can be viewed with:

```shell
$ sudo journalctl -u textgroup
```

## Advanced: Upgrading Textgroup

To create a new release that can be used to hot-upgrade the old one:

```shell
$ cd /tmp/textgroup-main
$ rebar3 as prod release
$ editor src/*.erl
$ sed -i s/0.1.0/0.2.0/ rebar.config src/textgroup.app.src
$ rebar3 as prod release
$ rebar3 as prod appup generate
$ rebar3 as prod relup -n textgroup -v 0.2.0
$ rebar3 as prod tar
```

The new release archive must then be copied into place (run this command and the
following ones as the `_textgroup` user):

```shell
$ cp /tmp/textgroup-main/_build/prod/rel/textgroup/textgroup-0.2.0.tar.gz /opt/textgroup/releases
```

Finally, the actual upgrade of the running service is performed like this:

```shell
$ /opt/textgroup/bin/textgroup upgrade 0.2.0
```

If the new release doesn't work as expected, a downgrade to the old one can be
performed:

```shell
$ /opt/textgroup/bin/textgroup downgrade 0.1.0
```

The unused release can then be removed. E.g., to uninstall the new one after
downgrading:

```shell
$ /opt/textgroup/bin/textgroup uninstall 0.2.0
```

A few caveats: If data structures (such as the `client_state` record) were
modified, converting them during the upgrade might require [additional
handling][appup_plugin]. Complex applications/dependencies may well need
explicit support for hot release upgrades, such as custom [appup][appup] files.
Also, if a different Erlang/OTP version is used to build a new release, the
upgrade process involves a [restart of the emulator][restart]. For this to
work, the [heart][heart] functionality must be enabled.

[erlang]: https://erlang.org
[rebar3]: https://rebar3.org
[release]: https://erlang.org/doc/design_principles/release_structure.html
[appup_plugin]: https://github.com/lrascao/rebar3_appup_plugin
[appup]: https://erlang.org/doc/design_principles/appup_cookbook.html
[restart]: https://erlang.org/doc/system_principles/upgrade.html
[heart]: https://erlang.org/doc/man/heart.html
