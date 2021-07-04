- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides = io
- infos/description =

# I/O binding for glib

For the purpose of I/O bindings please read the
[bindings readme](https://www.libelektra.org/bindings/readme#i-o-bindings).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `io-glib-elektra`.

## Requirements

- glib (version 2.x)

## Usage

Use the `elektraIoGlibNew` function to get a new I/O binding instance.
Make sure to build your application with `elektra-io-glib`, `elektra-io` and `glib` or
simply use `pkg-config --cflags --libs elektra-io-glib`.

### ElektraIoInterface _ elektraIoGlibNew (GMainContext _ context)

Create and initialize a new I/O binding.

_Parameters_

- context: Context to use for I/O operations. May be NULL to indicate the glib's
  default context.

_Returns_

Populated I/O interface

## Example

```C
#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio/glib.h>

#include <glib.h>

void main (void)
{
	KDB* repo;
	// ... open KDB

	// Create glib main loop
	GMainContext * context = NULL;
	GMainLoop * loop = g_main_loop_new (context, 0);

	// Initialize I/O binding tied to context
	ElektraIoInterface * binding = elektraIoGlibNew (context);

	// Set I/O binding
	elektraIoSetBinding (kdb, binding);

	// Start the event loop
	g_main_loop_run (loop);

	// Cleanup before exit
	elektraIoBindingCleanup (binding);
	g_main_loop_unref (loop);
}
```

Please check out the ["notificationReload" example](https://www.libelektra.org/examples/notificationreload)
which uses this I/O binding.
