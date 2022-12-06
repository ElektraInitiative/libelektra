/**
 * @file
 *
 * @brief Tests for I/O glib binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <elektra/kdbio.h>
#include <kdbiotest.h>
#include <tests.h>

#include <glib.h>

#include <kdbio/glib.h>

GMainLoop * loop;
GMainContext * context;

typedef struct
{
	GSource source;
	GPollFD pollFd;
} FdSource;

#define FD_READ_END 0
#define FD_WRITE_END 1

int fds[2];

static ElektraIoInterface * createBinding (void)
{
	return elektraIoGlibNew (context);
}

static void startLoop (void)
{
	g_main_loop_run (loop);
}

static void stopLoop (void)
{
	g_main_loop_quit (loop);
}

int main (int argc, char ** argv)
{
	context = NULL;

	init (argc, argv);

	context = g_main_context_new ();
	loop = g_main_loop_new (context, 0);

	elektraIoTestSuite (createBinding, startLoop, stopLoop);

	print_result ("iowrapper_glib");

	g_main_loop_unref (loop);
	g_main_context_unref (context);

	return nbError;
}
