/**
 * @file
 *
 * @brief module for calling the GPG binary
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "gpg.h"
#include <assert.h>
#include <errno.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static void closePipe (int * pipe)
{
	close (pipe[0]);
	close (pipe[1]);
}

/**
 * @brief call the gpg binary to perform the requested operation.
 *
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the message to be transformed
 * @param argv array holds the arguments passed on to the gpg process
 * @param argc contains the number of elements in argv, i.e. the size of argv
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGpgCall (Key * errorKey, Key * msgKey, char * argv[], size_t argc)
{
	pid_t pid;
	int status;
	int pipe_stdin[2];
	int pipe_stdout[2];
	kdb_octet_t * buffer = NULL;
	const ssize_t bufferSize = 2 * keyGetValueSize (msgKey);
	ssize_t outputLen;

	assert (argc > 1);

	// initialize pipes
	if (!pipe (pipe_stdin))
	{
		// TODO append errorKey
		return -1;
	}

	if (!pipe (pipe_stdout))
	{
		// TODO append errorKey
		closePipe (pipe_stdin);
		return -1;
	}

	// allocate buffer for gpg output
	// estimated maximum output size = 2 * input (including headers, etc.)
	if (!(buffer = elektraMalloc (bufferSize)))
	{
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		// TODO append errorKey
		return -1;
	}

	// sanitize the argument vector
	argv[0] = ELEKTRA_CRYPTO_DEFAULT_GPG_BIN;
	argv[argc - 1] = NULL;

	// fork into the gpg binary
	switch (pid = fork ())
	{
	case -1:
		// fork() failed
		// TODO append errorKey
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		elektraFree (buffer);
		return -1;

	case 0:
		// start of the forked child process
		close (pipe_stdin[1]);
		close (pipe_stdout[0]);

		// redirect stdin to pipe
		close (STDIN_FILENO);
		dup (pipe_stdin[0]);
		close (pipe_stdin[0]);

		// redirect stdout to pipe
		close (STDOUT_FILENO);
		dup (pipe_stdout[1]);
		close (pipe_stdout[1]);

		// finally call the gpg executable
		// TODO make gpg binary configurable
		if (execv (ELEKTRA_CRYPTO_DEFAULT_GPG_BIN, argv) < 0)
		{
			// TODO append errorKey
			return -1;
		}
		// end of the child process
	}

	// parent process
	close (pipe_stdin[0]);
	close (pipe_stdout[1]);

	// pass the message to the gpg process
	write (pipe_stdin[1], keyValue (msgKey), keyGetValueSize (msgKey));
	close (pipe_stdin[1]);

	// wait for the gpg process to finish
	waitpid (pid, &status, 0);

	// receive the output of the gpg process
	if (status == 0)
	{
		outputLen = read (pipe_stdout[0], buffer, bufferSize);
		keySetBinary (msgKey, buffer, outputLen);
	}
	close (pipe_stdout[0]);
	elektraFree (buffer);

	if (status != 0)
	{
		// gpg exited with an error
		// TODO append errorKey
		return -1;
	}
	return 1;
}
