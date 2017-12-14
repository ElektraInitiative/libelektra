# Notification Tutorial

## Preface

**The features described in this document are experimental or not implemented
yet. This document contains a preview of how developers would use the
notification feature**

This document explains how notifications are implemented in Elektra and how
they can be used by application developers.

## Notifications - Overview & Concept

Elektra's notification feature consists of several components.
While sending and receiving notifications is implemented by plugins,
applications use APIs provided by wrappers in order to use different plugins.

A wrapper for notifications provides the API for receiving and handling
notifications.
A wrapper for I/O operations allows asynchronous notification processing by
compatible plugins.
The I/O-wrapper consists of an *interface* used by transport plugins and a
*binding* which implements the actual I/O management functions for a specific
I/O management library.

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
notifications but not whether notifications are sent.
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

## How to integrate an I/O-binding and send notifications asynchronously

Developers do not need to change their programs in order to start sending
notifications.
However without the integration of an I/O-binding notifications are sent
synchronously which will block normal program execution.
For programs without time constraints (e.g. CLI programs) this may not be
important, but for GUIs or network services this will have negative impact.

Since many different I/O management libraries exist (e.g. libuv, glib or libev)
the transport plugins use the I/O-interface for their I/O tasks.
Each I/O management library needs its own I/O-binding.
Developers can also create their own I/O-binding for the I/O management library
of their choice.
This is described in the last section.

Each I/O-binding has its own initialization function that creates a new
I/O-binding and connects it to the I/O management library.
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

	// Initialize notification wrapper
	elektraNotificationOpen (kdb, binding);

	// Start the event loop
	uv_run (loop, UV_RUN_DEFAULT);

	// Cleanup
	elektraIoBindingCleanup (binding);
	uv_loop_close (loop);
	free (loop);
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
I/O-binding and initialized the notification-wrapper.
In order to handle change notifications a developer can either register a
variable or a callback.

### Register a variable

Values of registered variables are automatically updated when the value of the
assigned Key has changed.
In the following example we will register a integer variable:

```C
KDB * repo;

//  ... initialization of KDB and I/O-binding

Key * someKey = keyNew ("/sw/someprogram/v1/someKey", KEY_END);
int someKeyValue;

elektraNotificationRegisterInt (repo, &someKeyValue, someKey);

// repeatedly print variable
while (1) {
	sleep(2);

	KeySet * ks = ksNew (10, KS_END);
	kdbGet (repo, ks, someKey);
	Key * k = ksLookupByName (ks, "/sw/someprogram/v1/someKey", 0);
	if (k != 0)
	{
		printf ("value is %s\n", keyString (k));
	}
}
```

The variable `someKeyValue` will be automatically updated if the Key in the
program above is changed by another program (e.g. by using the `kdb` CLI
command).

### Callbacks

Registering a variable is suitable for programs where the Key's value is simply
displayed or used repeatedly (e.g. by a timer or in a loop).
When the program's initialization depends on the value of the Key (e.g. the
value sets the number of worker threads) updating a registered variable will
not suffice.
For these situations a callback should be used.

The following snippet shows how a callback can be used if the value of the
changed Key needs further processing.

```C
void myCallback (Key * changedKey)
{
	// do something
}

int main ()
{
	KDB * repo;

	// ... initialization of KDB and I/O-binding

	Key * someKey = keyNew ("/sw/someprogram/v1/someKey", KEY_END);

	elektraNotificationRegisterCallback(repo, &myCallback, someKey);

	// ...
}
```

## How to create your own I/O-binding

Developers can create their own bindings if the I/O management library of their
choice is not supported by an existing I/O-binding.

To create a binding the I/O-interface's functions pointers need to be passed to
`elektraIoNewBinding`.
The interface is independent from I/O management libraries and notification
transport plugins.

For details on see the
[example "doc" binding](https://github.com/ElektraInitiative/libelektra/blob/master/src/bindings/io/doc/)
or the [API documentation](https://doc.libelektra.org/api/current/html/kdbio_8h.html).
Existing I/O-bindings provide a good inspiration on how to implement a custom
binding.
Since a binding is generic and not application specific it is much appreciated
if you contribute your I/O-binding back to the Elektra project.

Elektra provides a test suite for I/O-bindings in order to make sure that
transport plugins will work with all bindings.
To run the test suite you need to execute `elektraIoTestSuite` and provide the
necessary callbacks for creating a new binding, starting and stopping
asynchronous processing.
