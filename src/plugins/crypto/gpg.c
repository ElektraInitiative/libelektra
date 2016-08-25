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
 * @brief concatenates dir and file.
 * @param errorKey holds an error description in case of failure.
 * @param dir contains the path to a directory
 * @param file contains a file name
 * @returns an allocated string containing "dir/file" which must be freed by the caller or NULL in case of error.
 */
static char * genGpgCandidate (Key * errorKey, char * dir, const char * file)
{
	const size_t resultLen = strlen (dir) + strlen (file) + 2;
	char * result = elektraMalloc (resultLen);
	if (!result)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return NULL;
	}
	snprintf (result, resultLen, "%s/%s", dir, file);
	return result;
}

/**
 * @brief lookup binary file bin in the PATH environment variable.
 * @param errorKey holds an error description in case of failure.
 * @param bin the binary file to look for
 * @param result holds an allocated string containing the full path to the binary file or NULL in case of error. Must be freed by the caller.
 * @retval -1 if an error occured. See errorKey for a description.
 * @retval 0 if the binary could not be found within PATH.
 * @retval 1 if the binary was found and the full path was stored in result.
 */
static int searchPathForBin (Key * errorKey, const char * bin, char ** result)
{
	*result = NULL;

	const char * envPath = getenv ("PATH");
	if (envPath)
	{
		const size_t envPathLen = strlen (envPath) + 1;
		char * dir;

		char * path = elektraMalloc (envPathLen);
		if (!path)
		{
			ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
			return -1;
		}
		memcpy (path, envPath, envPathLen);
		// save start of path as strsep() modifies path while splitting it up
		char * pathBegin = path;
		while ((dir = strsep (&path, ":")) != NULL)
		{
			char * candidate = genGpgCandidate (errorKey, dir, bin);
			if (!candidate)
			{
				elektraFree (pathBegin);
				return -1;
			}
			if (access (candidate, X_OK) == 0)
			{
				*result = candidate;
				elektraFree (pathBegin);
				return 1;
			}
			elektraFree (candidate);
		}
		elektraFree (pathBegin);
	}
	return 0;
}

/**
 * @brief lookup the path to the gpg binary in conf.
 * @param gpgBin holds allocated path to the gpg binary to be used or NULL in case of an error. Must bee freed by the caller.
 * @param conf KeySet holding the plugin configuration.
 * @param errorKey holds an error description if something goes wrong.
 * @retval 1 on success.
 * @retval -1 on error. In this case errorkey holds an error description.
 */
static int getGpgBinary (char ** gpgBin, KeySet * conf, Key * errorKey)
{
	*gpgBin = NULL;

	// plugin configuration has highest priority
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_BIN, 0);
	if (k)
	{
		const char * configPath = keyString (k);
		const size_t configPathLen = strlen (configPath);
		if (configPathLen > 0)
		{
			*gpgBin = elektraMalloc (configPathLen + 1);
			if (!(*gpgBin))
			{
				ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
				return -1;
			}
			strncpy (*gpgBin, configPath, configPathLen);
			return 1;
		}
	}

	// search PATH for gpg and gpg2 binaries
	switch (searchPathForBin (errorKey, "gpg2", gpgBin))
	{
	case 1: // success
		return 1;

	case -1: // error
		return -1;

	default: // not found
		break;
	}

	switch (searchPathForBin (errorKey, "gpg", gpgBin))
	{
	case 1: // success
		return 1;

	case -1: // error
		return -1;

	default: // not found
		break;
	}


	// last resort
	const size_t defaultLen = strlen (ELEKTRA_CRYPTO_DEFAULT_GPG_BIN);
	*gpgBin = elektraMalloc (defaultLen + 1);
	if (!(*gpgBin))
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}
	strncpy (*gpgBin, ELEKTRA_CRYPTO_DEFAULT_GPG_BIN, defaultLen);
	return 1;
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

	// sanitize the argument vector
	if (getGpgBinary (&argv[0], conf, errorKey) != 1)
	{
		return -1;
	}
	argv[argc - 1] = NULL;

	// check that the gpg binary exists and that it is executable
	if (access (argv[0], F_OK))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "gpg binary %s not found", argv[0]);
		elektraFree (argv[0]);
		return -1;
	}

	if (access (argv[0], X_OK))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "gpg binary %s has no permission to execute", argv[0]);
		elektraFree (argv[0]);
		return -1;
	}

	// initialize pipes
	if (pipe (pipe_stdin))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Pipe initialization failed");
		elektraFree (argv[0]);
		return -1;
	}

	if (pipe (pipe_stdout))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Pipe initialization failed");
		closePipe (pipe_stdin);
		elektraFree (argv[0]);
		return -1;
	}

	// allocate buffer for gpg output
	// estimated maximum output size = 2 * input (including headers, etc.)
	if (!(buffer = elektraMalloc (bufferSize)))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "Memory allocation failed");
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		elektraFree (argv[0]);
		return -1;
	}

	// fork into the gpg binary
	switch (pid = fork ())
	{
	case -1:
		// fork() failed
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "fork failed");
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		elektraFree (buffer);
		elektraFree (argv[0]);
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
	elektraFree (argv[0]);

	switch (status)
	{
	case 0:
		// everything ok
		return 1;

	case 1:
		// bad signature
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "GPG reported a bad signature");
		return -1;

	default:
		// other errors
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_GPG_FAULT, errorKey, "GPG failed with return value  %d", status);
		return -1;
	}
}
