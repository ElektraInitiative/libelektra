- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = notification
- infos/placements = postgetstorage postcommit
- infos/description = Sends DBus signals when a method is called

## Introduction ##

This plugin is a notification plugin which sends a signal to dbus when a
method is called. This plugin allows external programs to take action
when dbus notifies the program that a certain method has taken place
with Elektra.


## Dependencies ##

- `libdbus-1-dev`

## Dbus

A preferred way to interconnect desktop applications and even
embedded system applications on mobile devices running Linux is
D-Bus.  The idea of D-Bus accords to that of
Elektra: to provide standards to let software work together more tightly.
D-Bus provides a simple and lightweight IPC (Inter-Process
Communication= system to be used within desktop systems.  Next to
RPC (Remote Procedure Call), which is not used in this plugin,
it supports signals which can notify an arbitrary number of
other applications about changes.  Given software like a D-Bus library,
notification itself is a rather easy task, but it involves additional
library dependences.  So it is the perfect task to be implemented as
a plugin.  The information about the channels to be used can be stored
in the global key database.

D-Bus supports a **system-wide bus** and a **session bus**.
The system configuration can be accessed by each user and the user
configuration is limited to a single user. Both buses can immediately
be used for the system and user configuration notification updates to
get pleasing results.  But, there is a problem with the session bus:
It is possible within D-Bus that a user starts several sessions. The
user configuration should be global to the user and is not aware of
these sessions.  So if several sessions are started, some of the user's
processes will miss notification updates.

The namespaces are mapped to the buses the following way:

- system: system-wide bus
- user: session bus

## Usage ##

Mount the plugin additionally to a storage plugin, e.g.

	kdb mount file.dump / dump dbus

then we can receive the notification events using:

	dbus-monitor type='signal',interface='org.libelektra',path='/org/libelektra/configuration'

## Background

Today, programs are often interconnected in a dense way.  Such
applications should always be informed when something in their
environment changes.  For user interactive software, notification about
configuration changes is expected.  The only alternative is polling, which
wastes resources.  It additionally is no option, because for interactive
software the latency needs to be low.  Instead, the software which changes
the configuration has to notify all other interested applications that
can reread their configuration without significant delay.  In Elektra,
a notification plugin ensures that a notification is actually sent on
each change.

Applications can wait for such a notification with hand-written code.
Bindings, however, allow for better integration.  It is a common approach
for toolkits to provide a main loop.  Applications using such toolkits
can integrate notification services into this main loop.

The actions that occur in such events are application or toolkit specific
because of the non-invasive nature of Elektra.  Software reacts in many
different ways to update events.  Hence, the frequency of update events
should be kept at a minimum.  Changes are kept atomic with a single
attempt to write out configuration.  Notification callbacks shall
not change configuration because this can lead to a longer chain of
unwanted modifications.  That might not be true, however, if a programmer
of the whole system knows that a chain of reactions will terminate.
When doing such event-driven programming, care is needed to avoid
infinite loops.  Elektra guarantees consistency of the key database even
in such cases.
