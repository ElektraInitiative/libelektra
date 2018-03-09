- infos = Information about the zeromqrecv plugin is in keys below
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage
- infos/status = maintained unittest libc global experimental
- infos/description = Receives notifications using a ZeroMq subscriber socket

## Introduction

This plugin is a notification plugin, which receives notifications using ZeroMq
subscribe (`ZMQ_SUB`) sockets from the compatible zeromqsend plugin.

## Dependencies

- `libzmq3-dev` (ZeroMQ C bindings > 3.2)

## Usage

The recommended way is to globally mount the plugin together with the zeromqsend plugin:

	kdb global-mount zeromqsend zeromqrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
If notification is not enabled (i.e. in the tool `kdb` or in any other
application that does not use `elektraNotificationOpen()`) this plugin does
performs no operations.

# Transport Plugin

Mount this plugin globally with default settings to use it as *receiving*
transport plugin for Elektra's notification feature:

> kdb global-mount zeromqrecv

# Notification Format

For the notification format please see
[the `zeromqsend` plugin documentation](https://www.libelektra.org/plugins/zeromqsend#notification-format).
