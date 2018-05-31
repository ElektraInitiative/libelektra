- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides = io
- infos/description =

# I/O binding for glib

For the purpose of I/O bindings please read the
[bindings readme](https://www.libelektra.org/bindings/readme#i-o-bindings).

## Requirements

- glib (version 2.x)

## Usage

Use the `elektraIoGlibNew` function to get a new I/O binding instance.
Make sure to build your application with `elektra-io-glib`, `elektra-io` and `glib` or
simply use `pkg-config --cflags --libs elektra-io-glib`.

### ElektraIoInterface * elektraIoGlibNew (GMainContext * context)

Create and initialize a new I/O binding.

*Parameters*

- context: Context to use for I/O operations. May be NULL to indicate the glib's
  default context.

*Returns*

Populated I/O interface

## Example

```C

#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio_glib.h>

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
