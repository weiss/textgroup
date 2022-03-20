# Textgroup

Textgroup is a groupchat service usable with TCP clients such as Telnet or
Netcat.

## Requirements

- [Erlang/OTP][erlang] (22 or newer).
- [Rebar3][rebar3] (3.16.0 or newer).

## Building Textgroup

    $ rebar3 release

## Running Textgroup

    $ rebar3 shell

## Creating a Textgroup Release

    $ rebar3 as prod tar

## Deploying a Textgroup Release

    $ mkdir textgroup
    $ cd textgroup
    $ tar -xzf "$src_dir/_build/prod/rel/textgroup/textgroup-$version.tar.gz"

## Running a Textgroup Release

    $ bin/textgroup foreground

[erlang]: https://erlang.org
[rebar3]: https://rebar3.org
