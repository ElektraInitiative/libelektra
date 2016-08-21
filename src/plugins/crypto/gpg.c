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

static inline void closePipe (int * pipe)
{
	close (pipe[0]);
	close (pipe[1]);
}

/**
 * @brief lookup if the test mode for unit testing is enabled.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 test mode is not enabled
 * @retval 1 test mode is enabled
 */
static int inTestMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, 0);
	if (k && !strcmp (keyString (k), "1"))
	{
		return 1;
	}
	return 0;
}

/**
 * @brief lookup the path to the gpg binary in conf.
 * @param conf KeySet holding the plugin configuration.
 * @returns the path to the gpg binary to use. This value must not be modified!
 */
static char * getGpgBinary (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_BIN, 0);
	if (k)
	{
		const char * path = keyString (k);
		if (strlen (path) > 0)
		{
			// NOTE const is save to discard because the return value is not being modified
			return (char *)path;
		}
	}
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
	Key * k;

	// determine the number of total GPG keys to be used
	kdb_unsigned_short_t recipientCount = 0;
	kdb_unsigned_short_t testMode = 0;
	Key * root = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_KEY, 0);

	// check root key crypto/key
	if (root && strlen (keyString (root)) > 0)
	{
		recipientCount++;
	}

	// check for key beneath crypto/key (like crypto/key/#0 etc)
	ksRewind (conf);
	while ((k = ksNext (conf)) != 0)
	{
		if (keyIsBelow (k, root))
		{
			recipientCount++;
		}
	}

	if (recipientCount == 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey,
				    "Missing GPG key (specified as %s) in plugin configuration.", ELEKTRA_CRYPTO_PARAM_GPG_KEY);
		return -1;
	}

	if (inTestMode (conf))
	{
		// add two parameters for unit testing
		testMode = 2;
	}

	// initialize argument vector for gpg call
	const kdb_unsigned_short_t argc = (2 * recipientCount) + 3 + testMode;
	kdb_unsigned_short_t i = 1;
	char * argv[argc];

	// append root (crypto/key) as gpg recipient
	if (root && strlen (keyString (root)) > 0)
	{
		argv[i] = "-r";
		// NOTE argv[] values will not be modified, so const can be discarded safely
		argv[i + 1] = (char *)keyString (root);
		i = i + 2;
	}

	// append keys beneath root (crypto/key/#_) as gpg recipients
	ksRewind (conf);
	while ((k = ksNext (conf)) != 0)
	{
		if (keyIsBelow (k, root))
		{
			argv[i] = "-r";
			// NOTE argv[] values will not be modified, so const can be discarded safely
			argv[i + 1] = (char *)keyString (k);
			i = i + 2;
		}
	}

	// append option for unit tests
	if (testMode)
	{
		argv[argc - 4] = "--trust-model";
		argv[argc - 3] = "always";
	}

	argv[argc - 2] = "-e";

	// call gpg
	return elektraCryptoGpgCall (conf, errorKey, msgKey, argv, argc);
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
	if (inTestMode (conf))
	{
		char * argv[] = { "", "--trust-model", "always", "-d", NULL };
		return elektraCryptoGpgCall (conf, errorKey, msgKey, argv, 5);
	}
	else
	{
		char * argv[] = { "", "-d", NULL };
		return elektraCryptoGpgCall (conf, errorKey, msgKey, argv, 3);
	}
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
