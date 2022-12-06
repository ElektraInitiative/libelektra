/**
 * @file
 *
 * @brief Tests for I/O UV binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <elektra/kdbio.h>
#include <kdbiotest.h>
#include <tests.h>

#include <uv.h>

#include <kdbio/uv.h>

static ElektraIoInterface * createBinding (void)
{
	return elektraIoUvNew (uv_default_loop ());
}

static void startLoop (void)
{
	uv_run (uv_default_loop (), UV_RUN_DEFAULT);
}

static void stopLoop (void)
{
	uv_stop (uv_default_loop ());
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	elektraIoTestSuite (createBinding, startLoop, stopLoop);

	// Run loop once to fire handle closed callbacks and free memory
	// see http://docs.libuv.org/en/v1.x/handle.html#c.uv_close
	uv_loop_t * loop = uv_default_loop ();
	uv_run (loop, UV_RUN_ONCE);
#ifdef HAVE_LIBUV1
	uv_loop_close (loop);
#elif HAVE_LIBUV0
	uv_loop_delete (loop);
#endif

	print_result ("iowrapper_uv");

	return nbError;
}
