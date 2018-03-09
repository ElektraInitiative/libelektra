- infos = Information about the zeromqsend plugin is in keys below
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = maintained unittest libc global experimental
- infos/description = Sends notifications over ZeroMq publish sockets when a key is changed

## Introduction

This plugin is a notification plugin, which sends notifications using ZeroMq
publish (`ZMQ_PUB`) sockets when the key database (KDB) has been modified.
It is compatible with the sending zeromqrecv plugin.

## Dependencies

- `libzmq3-dev` (ZeroMQ C bindings > 3.2)

## Usage

The recommended way is to globally mount the plugin together with the zeromqrecv plugin:

> kdb global-mount zeromqsend zeromqrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
Since ZeroMq creates threads for asynchronous I/O this plugin always operates
asynchronously.

# Transport Plugin

Mount this plugin globally with default settings to use it as *sending*
transport plugin for Elektra's notification feature:

> kdb global-mount zeromqsend

# Notification Format

The ZeroMQ transport plugins use the publish/subscribe sockets (`ZMQ_PUB` and
`ZMQ_SUB`) for notification transport.

Each notification is a multipart message. The first part contains the type of
change, the second part contains the name of the changed key.

Possible change types are:

- KeyAdded
- KeyChanged
- KeyDeleted
