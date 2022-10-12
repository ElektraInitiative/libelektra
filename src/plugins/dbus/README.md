- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = notification
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = maintained unittest libc global
- infos/description = Sends notifications for every change via D-Bus

## Introduction

This plugin is a notification plugin, which sends a signal to D-Bus when
the key database (KDB) has been modified.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-dbus`.

## Dependencies

- `libdbus-1-dev`

## Dbus

A preferred way to interconnect desktop applications and even embedded
system applications on mobile devices running Linux is D-Bus. The idea
of D-Bus accords to that of Elektra: to provide standards to let software
work together more tightly. D-Bus provides a simple and lightweight IPC
(Inter-Process Communication) system to be used within desktop systems.
Next to RPC (Remote Procedure Call), which is not used in this plugin,
it supports signals which can notify an arbitrary number of other
applications about changes. Given software like a D-Bus library,
notification itself is a rather easy task, but it involves additional
library dependences. So it is the perfect task to be implemented as
a plugin. The information about the channels to be used can be stored
in the global key database.

D-Bus supports a **system-wide bus** and a **session bus**.
The system configuration can be accessed by each user and the user
configuration is limited to a single user. Both buses can immediately
be used for the system and user configuration notification updates to
get pleasing results. But, there is a problem with the session bus:
It is possible within D-Bus that a user starts several sessions. The
user configuration should be global to the user and is not aware of
these sessions. So if several sessions are started, some of the user's
processes will miss notification updates.

The namespaces are mapped to the buses the following way:

- system: system-wide bus
- user: session bus

Following signal names are used to notify about changes in the Elektra’s KeySet:

- KeyAdded: a key has been added
- KeyChanged: a key has been changed
- KeyDeleted: a key has been deleted

Alternatively, (with the option announce=once) only a single message is send:

- Commit: a key has been added, changed or deleted

## Usage

<!-- FIXME [new_backend]: outdated -->

The recommended way is to globally mount the plugin:

```sh
kdb global-mount dbus
```

Alternatively one can mount the plugin additionally to a storage plugin, e.g.:

```sh
kdb mount file.dump / dump dbus
```

For openicc one would use (mounts with announce=once):

```sh
kdb mount-openicc
```

### Shell

Then we can receive the notification events using:

```sh
dbus-monitor type='signal',interface='org.libelektra',path='/org/libelektra/configuration'
```

Or via the supplied test program:

```sh
kdb testmod_dbus receive_session
```

We can trigger a message with:

```sh
kdb set user:/dbus/x b
```

Note that changes in `user` fire on the dbus `session`,
and changes in namespace `system` in the dbus `system` bus.
To receive `system` changes we will use:

```sh
kdb testmod_dbus receive_system
dbus-monitor --system type='signal',interface='org.libelektra',path='/org/libelektra/configuration'
```

And then fire it with:

```sh
kdb set system:/dbus/y a
```

### C

```c
dbus_bus_add_match (connection, "type='signal',interface='org.libelektra',path='/org/libelektra/configuration'", &error);
```

See the full example [here](/src/plugins/dbus/receivemessage.c).

### Qt

Here a small example for QDBusConnection:

Place this in your Qt class header:

```cpp
public slots:
  void configChanged( QString msg );
```

Put this in your Qt class, e.g. the constructor:

```cpp
if( QDBusConnection::sessionBus().connect( QString(), "/org/libelektra/configuration", "org.libelektra", QString(),
                                       this, SLOT( configChanged( QString ) )) )
    fprintf(stderr, "=================== Done connect\n" );
```

Here comes the org.libelektra signals:

```cpp
void SynnefoApp::configChanged( QString msg )
{
  fprintf( stdout, "config changed: %s\n", msg.toLocal8Bit().data() );
};
```

### Python

In Python the DBus notifications can be used as follows

```python
import dbus
import gobject
gobject.threads_init()  # important: initialize threads if gobject main loop is used
from dbus.mainloop.glib import DBusGMainLoop

class DBusTest():
    def __init__(self):
        DBusGMainLoop(set_as_default=True)
        bus = dbus.SystemBus()  # may use session bus for user db
        bus.add_signal_receiver(self.elektra_dbus_key_changed_cb,
            signal_name="KeyChanged",
            dbus_interface="org.libelektra",
            path="/org/libelektra/configuration")

    def elektra_dbus_key_changed_cb(self, key):
        print('key changed %s' % key)

test = DBusTest()
loop = gobject.MainLoop()
try:
    loop.run()
except KeyboardInterrupt:
    loop.quit()
```

## Background

Today, programs are often interconnected in a dense way.
Such applications should always be informed when something in their
environment changes. For user interactive software, notification about
configuration changes is expected. The only alternative is polling, which
wastes resources. It additionally is no option for interactive software,
where the latency needs to be low. Instead, the software which changes
the configuration has to notify all other interested applications that
can reread their configuration without significant delay. In Elektra,
a notification plugin ensures that a notification is actually sent on
each change.

Applications can wait for such a notification with hand-written code.
Bindings, however, allow for better integration. It is a common approach
for toolkits to provide a main loop. Applications using such toolkits
can integrate notification services into this main loop.

The actions that occur in such events are application or toolkit specific
because of the non-invasive nature of Elektra. Software reacts in many
different ways to update events. Hence, the frequency of update events
should be kept at a minimum. Changes are kept atomic with a single
attempt to write out configuration. Notification callbacks shall
not change configuration because this can lead to a longer chain of
unwanted modifications. That might not be true, however, if a programmer
of the whole system knows that a chain of reactions will terminate.
When doing such event-driven programming, care is needed to avoid
infinite loops. Elektra guarantees consistency of the key database even
in such cases.

# Transport Plugin

Mount this plugin globally with default settings to use it as _sending_
transport plugin for Elektra's notification feature:

> kdb global-mount dbus announce=once

# Notification Format

This plugin uses D-Bus signal messages as notifications.
Notifications have the path `/org/libelektra/configuration` and the D-Bus
interface `org.libelektra`.
The following signal names are used:

- Commit: preferred, keys below the changed key have changed
- KeyAdded: a key has been added
- KeyChanged: a key has been changed

The first argument contains the name of the changed key.
The system bus is used if the affected keys is below the `system` namespace.
If the key is below the `user` namespace the session bus is used.

Example output from `dbus-monitor`:

```
signal time=1520805003.227723 sender=:1.8 -> destination=(null destination) serial=15 path=/org/libelektra/configuration; interface=org.libelektra; member=Commit
   string "system:/tests/foo"
```

## Problems

Key names that are not valid utf-8 cause a warning within the D-Bus library:

```
This is normally a bug in some application using the D-Bus library.
Couldn't add message argumentprocess 6139: arguments to dbus_message_iter_append_basic() were incorrect, assertion "_dbus_check_is_valid_utf8 (*string_p)" failed in file ../../dbus/dbus-message.c line 2676.
```
