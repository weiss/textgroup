# Textgroup

[![CI](https://github.com/weiss/textgroup/actions/workflows/ci.yml/badge.svg)][ci]

[Textgroup][textgroup] is a groupchat service usable with TCP clients such as
Telnet or Netcat. The purpose of this project is to serve as an example
application for Erlang/OTP newcomers.

## Requirements

- [Erlang/OTP][erlang] (22 or newer).
- [Rebar3][rebar3] (3.16.0 or newer).

## Quick Test

The Textgroup service can be compiled and started [with an Erlang shell][shell]
by running the following command:

```shell
$ rebar3 shell
```

The service is stopped by calling `q().` within that shell.

## Continuous Integration

The results of continuous integration tests can be viewed [on GitHub][ci],
including test suite [logs][ct_logs] and [coverage][coverage] analysis.

## Documentation

There's documentation for [users][users], [operators][ops], and
[developers][devs] on the Textgroup [web site][textgroup].

[textgroup]: https://weiss.github.io/textgroup/
[erlang]: https://erlang.org
[rebar3]: https://rebar3.org
[shell]: https://ferd.ca/rebar3-shell.html
[ci]: https://github.com/weiss/textgroup/actions/workflows/ci.yml
[ct_logs]: https://weiss.github.io/textgroup/logs/
[coverage]: https://weiss.github.io/textgroup/cover/
[users]: https://weiss.github.io/textgroup/users.html
[ops]: https://weiss.github.io/textgroup/operators.html
[devs]: https://weiss.github.io/textgroup/developers.html
