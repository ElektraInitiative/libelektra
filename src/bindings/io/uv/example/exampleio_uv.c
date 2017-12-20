/**
 * @file
 *
 * @brief Example program for io_uv binding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * This program uses two operations:
 * - The "input" operation is a file descriptor watcher that waits for
 *   STDIN_FILENO (stdin) to become readable.
 *   Since input is buffered, this typically happends when the user enters some
 *   text and presses return.
 * - The "output" operation is a timer that prints the last read data every
 *   second.
 */
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <kdb.h>
#include <kdbhelper.h>
#include <kdbio.h>
#include <kdbio_uv.h>

#include <uv.h>

#define BUFFER_LENGTH 255
#define ONE_SECOND 1000
#define MIN(a, b) (((a) < (b)) ? (a) : (b))

ElektraIoInterface * binding;
ElektraIoFdOperation * input;
ElektraIoTimerOperation * output;

void stopLoop (void)
{
	// Cleanup
	elektraIoBindingRemoveFd (input);
	elektraIoBindingRemoveTimer (output);
	elektraFree (input);
	elektraFree (output);
	elektraIoBindingCleanup (binding);

	uv_stop (uv_default_loop ());
}

void readText (ElektraIoFdOperation * fdOp, int flags ELEKTRA_UNUSED)
{
	printf ("input: file descriptor became readable\n");

	char * lastInput = elektraIoFdGetData (fdOp);
	assert (lastInput != NULL);

	char buffer[BUFFER_LENGTH];
	int bytesRead = read (elektraIoFdGetFd (fdOp), &buffer, BUFFER_LENGTH);
	if (bytesRead != -1)
	{
		// make sure there is a null terminator in buffer
		buffer[MIN (BUFFER_LENGTH - 1, bytesRead + 1)] = 0;
		// remove newline from string
		buffer[strcspn (buffer, "\r\n")] = 0;
		// copy to lastInput
		memcpy (lastInput, buffer, BUFFER_LENGTH);
	}
	else
	{
		int error = errno;
		if (error != EINTR)
		{
			printf ("input: I/O error occured - exiting\n");
			stopLoop ();
		}
	}
}

void printText (ElektraIoTimerOperation * timerOp)
{
	char * lastInput = elektraIoTimerGetData (timerOp);
	assert (lastInput != NULL);

	if (strcmp (lastInput, "exit") == 0)
	{
		printf ("timer: stopping\n");
		stopLoop ();
	}
	else
	{
		if (strlen (lastInput) > 0)
		{
			printf ("timer: last text was \"%s\"\n", lastInput);
		}
	}
}

int main (void)
{
	// Initialize buffer
	char lastInput[BUFFER_LENGTH];
	memset (lastInput, 0, BUFFER_LENGTH);

	printf ("Please enter some text and press return.\n");
	printf ("Enter \"exit\" to stop and exit.\n");

	// Create libuv event loop
	uv_loop_t * loop = uv_default_loop ();

	// Initialize I/O binding tied to event loop
	binding = elektraIoUvNew (loop);
	// Read lines from STDIN
	input = elektraIoNewFdOperation (STDIN_FILENO, ELEKTRA_IO_READABLE, 1, readText, &lastInput);
	// Print last read data every second
	output = elektraIoNewTimerOperation (ONE_SECOND, 1, printText, &lastInput);

	// Add operations to binding
	elektraIoBindingAddFd (binding, input);
	elektraIoBindingAddTimer (binding, output);

	// Start the event loop
	uv_run (loop, UV_RUN_DEFAULT);

#ifdef HAVE_LIBUV1
	uv_loop_close (loop);
#endif
#ifdef HAVE_LIBUV0
	uv_loop_delete (loop);
#endif

	return 0;
}
