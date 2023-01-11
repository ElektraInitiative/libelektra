- infos = Information about the zeromqrecv plugin is in keys below
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage
- infos/status = maintained tested/unit hook experimental
- infos/description = Receives notifications using a ZeroMq subscriber socket

## Introduction

This plugin is a notification plugin, which receives notifications using ZeroMq
subscribe (`ZMQ_SUB`) sockets from the compatible zeromqsend plugin.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-zeromq`.

## Dependencies

- `libzmq3-dev` (ZeroMQ C bindings > 3.2)

## Usage

<!-- FIXME [new_backend]: outdated -->

The recommended way is to globally mount the plugin together with the zeromqsend plugin:

    kdb global-mount zeromqsend zeromqrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
If notification is not enabled (i.e. in the tool `kdb` or in any other
application that does not use `elektraNotifiationContract()`) this plugin does
performs no operations.

Since ZeroMQ sockets only provide a 1:n mapping (i.e. one publisher with many
subscribers or one subscriber and many publishers) the `zeromqsend` and
`zeromqrecv` plugins require a XPUB/XSUB endpoint.
The kdb tool ["hub-zeromq"](https://www.libelektra.org/tools/hub-zeromq)
provides these endpoints.

# Transport Plugin

<!-- FIXME [new_backend]: outdated -->

Mount this plugin globally with default settings to use it as _receiving_
transport plugin for Elektra's notification feature:

> kdb global-mount zeromqrecv

# Configuration

This plugin supports the following configuration options when mounting:

- **endpoint**: ZeroMQ XPUB or PUB socket to connect to. The
  [`ipc`](http://api.zeromq.org/4-2:zmq-ipc) and
  [`tcp`](http://api.zeromq.org/4-2:zmq-tcp) ZeroMQ transports are recommended.
  The default value is "tcp://localhost:6001".

# Notification Format

For the notification format please see
[the `zeromqsend` plugin documentation](https://www.libelektra.org/plugins/zeromqsend#notification-format).
