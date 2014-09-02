- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = notification
- infos/placements = postcommit
- infos/description = Prints timestamps when a method is called.

## Introduction ##

This plugin is a notification plugin which sends a signal to dbus when a
method is called. This plugin allows external programs to take action
when dbus notifies the program that a certain method has taken place
with Elektra.

## Usage ##

Mount the plugin additionally to a storage plugin, e.g.

	kdb mount file.dump / dump dbus

then we can receive the notification events using:

	dbus-monitor type='signal',interface='org.libelektra',path='/org/libelektra/configuration'
