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

#include <kdbio.h>
#include <kdbiotest.h>
#include <tests.h>

#include <glib.h>

#include "kdbio_glib.h"

#include <unistd.h> // pipe()

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

int timerHandler (void * data)
{
	printf ("timerHandler %p\n", data);
	char * message = (char *)data;
	printf ("timerHandler message: %s\n\n", message);
	write (fds[FD_WRITE_END], "ahoi", 4);
	return G_SOURCE_CONTINUE;
}

int idleHandler (void * data)
{
	printf ("idleHandler %p\n", data);
	char * message = (char *)data;
	printf ("idleHandler message: %s\n\n", message);
	return G_SOURCE_CONTINUE;
}

int fd_prepare (GSource * source, gint * timeout)
{
	FdSource * fd = (FdSource *)source;
	*timeout = -1; // we wait until data is available
	int ret = !!(fd->pollFd.events & fd->pollFd.revents);
	printf ("fd_prepare returns %s\n", ret ? "true" : "false");
	return ret;
}

int fd_check (GSource * source)
{
	FdSource * fd = (FdSource *)source;
	int ret = !!(fd->pollFd.events & fd->pollFd.revents);
	printf ("fd_check returns %s\n", ret ? "true" : "false");
	return ret;
}

int fd_dispatch (GSource * source, GSourceFunc callback, void * data)
{
	FdSource * fd = (FdSource *)source;
	if ((fd->pollFd.revents & G_IO_OUT))
	{
		// dispatch
		printf ("fd_dispatch: fd is writable\n");
		// TODO write into buffer
		// delete returned flag
		fd->pollFd.revents &= ~G_IO_OUT;
	}

	if ((fd->pollFd.revents & G_IO_IN))
	{
		// dispatch
		printf ("fd_dispatch: fd is readable\n");
		// TODO read into buffer
		char buffer[5];
		read (fds[FD_READ_END], &buffer, 5);
		printf ("fd_dispatch read: %s\n", buffer);
		// delete returned flag
		fd->pollFd.revents &= ~G_IO_IN;
	}

	if (callback)
	{
		printf ("fd_dispatch: callback present\n");
		return callback (data);
	}

	return G_SOURCE_CONTINUE;
}

void fd_finalize (GSource * source)
{
	FdSource * fd = (FdSource *)source;

	// Stop polling our fd
	if (fd->pollFd.fd >= 0)
	{
		g_source_remove_poll (source, &fd->pollFd);
		fd->pollFd.fd = -1;
	}
}

int main (int argc, char ** argv)
{
	context = NULL;

	init (argc, argv);

	loop = g_main_loop_new (context, 0);

	// elektraIoTestSuite (createBinding, startLoop, stopLoop);
	elektraIoTestSuiteIdle (createBinding, startLoop, stopLoop);

	// Tryouts
	/*char * data = "BARFOO!";
	GSource * timer = g_timeout_source_new (1000);
	g_source_set_callback (timer, timerHandler, data, NULL);
	g_source_attach (timer, context);

	GSource * idle = g_idle_source_new ();
	g_source_set_callback (idle, idleHandler, data, NULL);
	// g_source_attach (idle, context);

	if (pipe (fds) == -1)
	{
		printf ("pipe() failed");
		exit (-1);
	}
	GSourceFuncs funcs = {.prepare = fd_prepare, .check = fd_check, .dispatch = fd_dispatch, .finalize = fd_finalize };
	// Eigenes struct anstelle von *fd möglich, GSource muss an erster Stelle stehen
	FdSource * fd = g_source_new (&funcs, sizeof *fd);
	fd->pollFd.fd = fds[FD_READ_END];
	fd->pollFd.events = G_IO_IN;
	fd->pollFd.revents = 0;
	g_source_add_poll ((GSource *)fd, &fd->pollFd);
	g_source_attach ((GSource *)fd, context);

	g_main_loop_run (loop);*/

	print_result ("iowrapper_glib");

	g_main_loop_unref (loop);

	return nbError;
}
