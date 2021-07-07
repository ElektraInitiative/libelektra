- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides = io
- infos/description =

# I/O binding for libuv

For the purpose of I/O bindings please read the
[bindings readme](https://www.libelektra.org/bindings/readme#i-o-bindings).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `io-uv-elektra`.

## Requirements

- [libuv](http://libuv.org/) (version 1.x is recommended; 0.10 is supported)

## Usage

Use the `elektraIoUvNew` function to get a new I/O binding instance.
Make sure to build your application with `elektra-io-uv`, `elektra-io` and `uv` or
simply use `pkg-config --cflags --libs elektra-io-uv`.

### ElektraIoInterface _ elektraIoUvNew (uv_loop_t _ loop)

Create and initialize a new I/O binding.

_Parameters_

- loop: Loop to use for I/O operations

_Returns_

Populated I/O interface

## Example

```C
#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio/uv.h>

#include <uv.h>

void main (void)
{
	KDB* repo;
	// ... open KDB

	// Create libuv event loop
	uv_loop_t * loop = uv_default_loop ();

	// Initialize I/O binding tied to event loop
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	// Set I/O binding
	elektraIoSetBinding (kdb, binding);

	// Start the event loop
	uv_run (loop, UV_RUN_DEFAULT);

	// Cleanup before exit
	elektraIoBindingCleanup (binding);
	uv_loop_close (loop);
}
```

Please check out the ["notificationAsync" example](https://www.libelektra.org/examples/notificationasync)
which uses this I/O binding.
