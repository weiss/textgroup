# For Users

Textgroup can be used with TCP clients such as Telnet or Netcat. For example:

```sh
$ telnet localhost 1111
```

Once connected, the server understands the following commands:

## peers

Show the IP addresses of your current peers.

## stats

Show some statistic regarding the current session.

## help

Show a help message.

## quit

Quit the current session.

_Any other messages are sent to all connected peers._
