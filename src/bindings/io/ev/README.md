- infos =
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides = io
- infos/description =

# I/O binding for libev

For the purpose of I/O bindings please read the
[bindings readme](https://www.libelektra.org/bindings/readme#i-o-bindings).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `io-ev-elektra`.

## Requirements

- [libev](http://libev.schmorp.de) (4.x; tested with 4.22)

## Usage

Use the `elektraIoEvNew` function to get a new I/O binding instance.
Make sure to build your application with `elektra-io-ev`, `elektra-io` and `ev` or
simply use `pkg-config --cflags --libs elektra-io-ev`.

### ElektraIoInterface _ elektraIoEvNew (struct ev_loop _ loop)

Create and initialize a new I/O binding.

_Parameters_

- loop: Loop to use for I/O operations

_Returns_

Populated I/O interface

## Example

```C
#include <elektra/kdb.h>
#include <elektra/kdbio.h>
#include <elektra/kdbio/ev.h>

#include <ev.h>

void main (void)
{
	KDB * repo;
	// ... open KDB

	// Create libuv event loop
	struct ev_loop * loop = EV_DEFAULT;

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
