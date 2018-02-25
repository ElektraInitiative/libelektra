- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = maintained unittest libc global
- infos/description = Sends DBus signals when a method is called

## Introduction

This plugin is a notification plugin, which receives a signal from D-Bus when
the key database (KDB) has been modified.
It is compatible with the sending D-Bus plugin.

## Dependencies

- `libdbus-1-dev`

## Usage

The recommended way is to globally mount the plugin together with the dbus plugin:

	kdb global-mount dbus dbusrecv

This plugin is designed to be used as a transport plugin for Elektra's
notification feature.
If notification is not enabled (i.e. in the tool `kdb` or in any other
application that does not use `elektraNotificationOpen()`) this plugin performs
no actions.

This plugin cannot be directly used to receive notifications.
Applications that use the notification feature implicitly use it when this
plugin is mounted globally.
