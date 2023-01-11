- infos = Information about the dbus plugin is in keys below
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage
- infos/status = maintained tested/unit hook experimental
- infos/description = Receives notifications via D-Bus

## Introduction

This plugin is a notification plugin, which receives a signal from D-Bus when
the key database (KDB) has been modified.
It is compatible with the sending D-Bus plugin.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-dbus`.

## Dependencies

- `libdbus-1-dev`

## Usage

<!-- FIXME [new_backend]: outdated -->

The recommended way is to globally mount the plugin together with the dbus plugin:

```sh
kdb global-mount dbus dbusrecv
```

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
If notification is not enabled (i.e. in the tool `kdb` or in any other
application that does not use `elektraNotifiationContract()`) this plugin performs
no actions.

This plugin cannot be directly used to receive notifications.
Applications that use the notification feature implicitly use it when this
plugin is mounted globally.

# Transport Plugin

<!-- FIXME [new_backend]: outdated -->

Mount this plugin globally with default settings to use it as _receiving_
transport plugin for Elektra's notification feature:

> kdb global-mount dbusrecv

For the message format please see
[the `dbus` plugin documentation](https://www.libelektra.org/plugins/dbus#notification-format).
