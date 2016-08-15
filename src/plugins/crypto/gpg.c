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
#include <kdberrors.h>
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

static char * getGpgBinary (KeySet * conf)
{
	return ELEKTRA_CRYPTO_DEFAULT_GPG_BIN;
}

/**
 * @brief call the gpg binary to encrypt the random master password r.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the master password to be encrypted
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGpgEncryptMasterPassword (KeySet * conf, Key * errorKey, Key * msgKey)
{
	char * argv[] = { "", "-r", "859E2AD7", "-e", NULL };
	// TODO use conf to determine the recipient
	return elektraCryptoGpgCall (conf, errorKey, msgKey, argv, 5);
}

/**
 * @brief call the gpg binary to decrypt the random master password r.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the master password to be decrypted. Note that the content of this key will be modified.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGpgDecryptMasterPassword (KeySet * conf, Key * errorKey, Key * msgKey)
{
	char * argv[] = { "", "-d", NULL };
	return elektraCryptoGpgCall (conf, errorKey, msgKey, argv, 3);
}

/**
 * @brief call the gpg binary to perform the requested operation.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the message to be transformed
 * @param argv array holds the arguments passed on to the gpg process
 * @param argc contains the number of elements in argv, i.e. the size of argv
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGpgCall (KeySet * conf, Key * errorKey, Key * msgKey, char * argv[], size_t argc)
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
	if (pipe (pipe_stdin))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Pipe initialization failed");
		return -1;
	}

	if (pipe (pipe_stdout))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Pipe initialization failed");
		closePipe (pipe_stdin);
		return -1;
	}

	// allocate buffer for gpg output
	// estimated maximum output size = 2 * input (including headers, etc.)
	if (!(buffer = elektraMalloc (bufferSize)))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Memory allocation failed");
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		return -1;
	}

	// sanitize the argument vector
	argv[0] = getGpgBinary (conf);
	argv[argc - 1] = NULL;

	// fork into the gpg binary
	switch (pid = fork ())
	{
	case -1:
		// fork() failed
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "fork failed");
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
		if (execv (argv[0], argv) < 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "failed to start the gpg binary: %s", argv[0]);
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
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "GPG returned %d", status);
		return -1;
	}
	return 1;
}
