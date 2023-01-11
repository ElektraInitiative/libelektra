- infos = Information about the zeromqsend plugin is in keys below
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = maintained tested/unit hook experimental
- infos/description = Sends notifications over ZeroMq publish sockets when a key is changed

## Introduction

This plugin is a notification plugin, which sends notifications using ZeroMq
publish (`ZMQ_PUB`) sockets when the key database (KDB) has been modified.
It is compatible with the sending zeromqrecv plugin.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-zeromq`.

## Dependencies

- `libzmq3-dev` (ZeroMQ C bindings > 3.2)

## Usage

<!-- FIXME [new_backend]: outdated -->

The recommended way is to globally mount the plugin together with the zeromqrecv plugin:

> kdb global-mount zeromqsend zeromqrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
Since ZeroMq creates threads for asynchronous I/O this plugin always operates
asynchronously.

Since ZeroMQ sockets only provide a 1:n mapping (i.e. one publisher with many
subscribers or one subscriber and many publishers) the `zeromqsend` and
`zeromqrecv` plugins require a XPUB/XSUB endpoint.
The kdb tool ["hub-zeromq"](https://www.libelektra.org/tools/hub-zeromq)
provides these endpoints.

# Transport Plugin

<!-- FIXME [new_backend]: outdated -->

Mount this plugin globally with default settings to use it as _sending_
transport plugin for Elektra's notification feature:

> kdb global-mount zeromqsend

# Configuration

This plugin supports the following configuration options when mounting:

- **endpoint**: ZeroMQ XSUB or SUB socket to connect to. The
  [`ipc`](http://api.zeromq.org/4-2:zmq-ipc) and
  [`tcp`](http://api.zeromq.org/4-2:zmq-tcp) ZeroMQ transports are recommended.
  The default value is "tcp://localhost:6000".
- **connectTimeout**: Timeout for establishing connections in milliseconds. The default value is "1000".
- **subscribeTimeout**: Timeout for waiting for subscribers in milliseconds. The default value is "200".

# Notification Format

The ZeroMQ transport plugins use the publish/subscribe sockets (`ZMQ_PUB` and
`ZMQ_SUB`) for notification transport.

Each notification is a multipart message. The first part contains the type of
change, the second part contains the name of the changed key.

Possible only current change is `Commit`.
