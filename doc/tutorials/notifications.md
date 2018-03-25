# Notification Tutorial

## Preface

**The features described in this document are experimental.**

This document explains how notifications are implemented in Elektra and how
they can be used by application developers.

## Notifications - Overview & Concept

Elektra's notification feature consists of several components.
While sending and receiving notifications is implemented by plugins,
applications use APIs provided by wrappers in order to use different plugins.

A
[wrapper for notifications](https://doc.libelektra.org/api/current/html/group__kdbnotification.html)
provides the API for receiving and handling notifications.
A [wrapper for I/O operations](https://doc.libelektra.org/api/current/html/group__kdbio.html)
allows asynchronous notification processing by compatible plugins.
The I/O wrapper consists of an *interface* used by transport plugins and
multiple implementations of that interface called *I/O bindings*.
A I/O binding implements the actual I/O management functions for a specific
I/O management library.
Applications typically use one I/O binding but can also use none or multiple
I/O bindings.

Transport plugins exchange notifications via different protocols like D-Bus,
Redis and ZeroMQ.
For each type of transport there are two types of plugins: One for sending and
one for receiving notifications.
Developers do not interact with those plugins directly.
The underlying transports are transparent to them.
An internal-notification plugin implements notification handling functions and
feeds back configuration changes from within the application.

![Overview](../images/notifications.svg)

When a configuration key is changed Elektra can generate change notifications
that allow applications to process those changes.
Developers can choose whether and how they want to receive and handle those
notifications but not whether notifications are sent or which transport is used.
How notifications are sent is specified in the *notification configuration* by
the system operator.

## Notification configuration

System operators can mount the desired transport plugins and configure them
(e.g. set channel, host, port and credentials) either globally or when mounting
a configuration file.

They need to mount both sending and receiving plugins in order to use a
transport.

```sh
kdb mount file.ini system/example ini dbussend dbusreceive
```

## How to integrate an I/O binding and send notifications asynchronously

Developers do not need to change their programs in order to start sending
notifications.
However without the integration of an I/O binding notifications are sent
synchronously which will block normal program execution.
For programs without time constraints (e.g. CLI programs) this may not be
important, but for GUIs or network services this will have negative impact.

Since many different I/O management libraries exist (e.g. libuv, glib or libev)
the transport plugins use the I/O interface for their I/O operations.
Each I/O management library needs its own I/O binding.
Developers can also create their own I/O binding for the I/O management library
of their choice.
This is described in the last section.

Each I/O binding has its own initialization function that creates a new
I/O binding and connects it to the I/O management library.
For this tutorial we will assume that libuv is used.
For details on how to use a specific binding please look at the bindings'
README.md in `src/bindings/io/<binding>`.

```C
#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio_uv.h>
#include <elektra/kdbnotification.h>

#include <uv.h>

void main (void)
{
	KDB* repo;

	// .. open KDB

	// Create libuv event loop
	uv_loop_t * loop = uv_default_loop ();

	// Initialize I/O binding tied to event loop
	ElektraIoInterface * binding = elektraIoUvNew (loop);

  // Use I/O binding for our kdb instance
  elektraIoSetBinding (kdb, binding);

	// Initialize notification wrapper
	elektraNotificationOpen (kdb);

	// Start the event loop
	uv_run (loop, UV_RUN_DEFAULT);

	// Cleanup
  elektraNotificationClose (kdb);
  elektraIoBindingCleanup (binding);
	uv_loop_close (loop);
}

void someFunction (void)
{
	// notifications are sent asynchronously on kdbSet
	// over configured transports using our uv event loop
	kdbSet (repo, key, parentKey);
}
```

## How to receive notifications

We extend the example from the previous section where we already created our
I/O binding and initialized the notification-wrapper.
In order to handle change notifications a developer can either register a
variable or a callback.

### Register a variable

Values of registered variables are automatically updated when the value of the
assigned key has changed.
In the following example we will register an integer variable:

```C
KDB * repo;

//  ... initialization of KDB and I/O binding

Key * key = keyNew ("/sw/myorg/myprogram/#0/current/value", KEY_END);
int keyValue;

int result = elektraNotificationRegisterInt (repo, key, &keyValue);
if (!result)
{
	printf ("could not register variable!\n");
	return;
}

// repeatedly print variable
while (1) {
	printf ("value is %d\n", keyValue);

	sleep(2);
}
```

After calling `elektraNotificationRegisterInt` the variable `keyValue` will be
automatically updated if the key in the program above is changed by another
program (e.g. by using the `kdb` CLI command).
For automatic updates to work transport plugins have to be in place either at
a mountpoint above the configuration or mounted globally.

### Callbacks

Registering a variable is suitable for programs where the key's value is simply
displayed or used repeatedly (e.g. by a timer or in a loop).
If an initialization code needs to be redone after configuration changes (e.g. a
value sets the number of worker threads) updating a registered variable will
not suffice.
For these situations a callback should be used.

The following snippet shows how a callback can be used if the value of the
changed key needs further processing.

```C
#include <signal.h>
#include <stdio.h>
#include <string.h>

// from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#define ANSI_COLOR_RED			"\x1b[31m"
#define ANSI_COLOR_GREEN		"\x1b[32m"

void setTerminalColor (Key * color, void * context ELEKTRA_UNUSED)
{
  // context contains whatever was passed as 4th parameter
  // to elektraNotificationRegisterCallback()
	char * value = keyString (color);

	if (strcmp (value, "red") == 0)
	{
		printf (ANSI_COLOR_RED);
	}
	if (strcmp (value, "green") == 0)
	{
		printf (ANSI_COLOR_GREEN);
	}
}

int main (void)
{
	KDB * repo;

	// ... initialization of KDB, I/O binding and notifications

	Key * configBase = keyNew ("/sw/myorg/myprogram/#0/current", KEY_END);
	Key * color = keyNew ("/sw/myorg/myprogram/#0/current/color", KEY_END);

	// Retrieve key from kdb
	KeySet * ks = ksNew (10, KS_END);
	kdbGet (repo, ks, configBase);
	Key * key = ksLookup (ks, color, 0);
	if (key) {
		// Initialization
		setTerminalColor (key);
	}

	// Re-Initialize on key changes
	int result = elektraNotificationRegisterCallback(repo, color, &setTerminalColor, NULL);
	if (!result) {
		printf ("could not register callback!\n");
		return -1;
	}

	// ... start loop, etc.
}
```

## How to create your own I/O Binding

Developers can create their own bindings if the I/O management library of their
choice is not supported by an existing I/O binding.

For details on see the [example "doc" binding](/src/bindings/io/doc/) or the
[API documentation](https://doc.libelektra.org/api/current/html/group__kdbio.html).
Existing I/O bindings provide a good inspiration on how to implement a custom
binding.
Since a binding is generic and not application specific it is much appreciated
if you contribute your I/O binding back to the Elektra project.

## Logging

In order to log and analyze application behavior the logging plugins
["syslog"](https://www.libelektra.org/plugins/syslog),
["journald"](https://www.libelektra.org/plugins/journald) or
["logchange"](https://www.libelektra.org/plugins/logchange) can be used.
In order to log not only `kdbSet()` but also `kdbGet()` the option `log/get=1`
should be used when mounting these plugins.

For example:

```
$ kdb global-mount syslog log/get=1
```
