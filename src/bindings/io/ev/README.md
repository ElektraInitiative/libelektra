- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides = io
- infos/description =

# I/O binding for libev

## Requirements

- [libev](http://libev.schmorp.de) (tested with 4.x)

## Usage

Use the `elektraIoEvNew` function to get a new I/O binding instance.
Make sure to build your application with `elektra-io-ev`, `elektra-io` and `ev` or
simply use `pkg-config --cflags --libs elektra-io-ev`.

### ElektraIoInterface * elektraIoEvNew (struct ev_loop * loop)

Create and initialize a new I/O binding.

*Parameters*

- loop Loop to use for I/O operations

*Returns*

Populated I/O interface

## Example

```C

#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio_ev.h>

#include <ev.h>

void main (void)

{
	KDB* repo;
	// ... open KDB

	// Create libuv event loop
	struct ev_loop * loop = EV_DEFAULT

	// Initialize I/O binding tied to event loop
	ElektraIoInterface * binding = elektraIoEvNew (loop);

	// Set I/O binding
	elektraIoSetBinding (kdb, binding);

	// Start the event loop
	ev_run (loop, 0);

	// Cleanup before exit
	elektraIoBindingCleanup (binding);
	ev_loop_destroy (EV_DEFAULT);
}

```
