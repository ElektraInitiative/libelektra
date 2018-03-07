- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = maintained unittest libc global experimental
- infos/description = Sends DBus signals when a method is called

## Introduction

This plugin is a notification plugin, which receives a signal from D-Bus when
the key database (KDB) has been modified.
It is compatible with the sending D-Bus plugin.

## Dependencies

- `libzmq` (TODO check compatible versions)

## Usage

The recommended way is to globally mount the plugin together with the zeromqrecv plugin:

	kdb global-mount zeromqsend zeromqrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
If notification is not enabled (i.e. in the tool `kdb` or in any other
application that does not use `elektraNotificationOpen()`) this plugin falls
back to synchronous sending.

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
