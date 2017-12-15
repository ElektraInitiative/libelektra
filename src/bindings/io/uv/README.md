# IO-Binding for libuv

## Infos
- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental io
- infos/description = IO-Binding for libuv

## Requirements

- libuv (version 1.x is recommended)

## Usage

Use the `elektraIoUvNew` function to get a new binding.
Make sure to build your application with `elektra-io-uv`, `elektra-io` and `uv` or
simply use `pkg-config --cflags --libs elektra-io-uv`.

### ElektraIoInterface * elektraIoUvNew (uv_loop_t * loop)

Create and initialize a new IO binding.

*Parameters*

- loop Loop to use for IO operations

*Returns*

Populated IO interface

## Example

```C

#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio_uv.h>
#include <elektra/kdbnotification.h>

#include <uv.h>

void main (void)

{
	KDB* repo;
	// ... open KDB

	// Create libuv event loop
	uv_loop_t * loop = uv_default_loop ();

	// Initialize I/O binding tied to event loop
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	// Initialize notification wrapper
	elektraNotificationOpen (kdb, binding);

	// Start the event loop
	uv_run (loop, UV_RUN_DEFAULT);

	// Cleanup before exit
	elektraIoBindingCleanup (binding);
	uv_loop_close (loop);
	free (loop);
}

```
