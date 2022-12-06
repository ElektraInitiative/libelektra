/**
 * @file
 *
 * @brief module for calling the GPG binary
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "gpg.h"
#include <assert.h>
#include <errno.h>
#include <kdberrors.h>
#include <elektra/kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define GPG_OUTPUT_DEFAULT_BUFFER_SIZE 1024
#define GPG_MAX_KEYID_LENGTH 32
#define GPG_ERROR_MISSING_KEY_LIST                                                                                                         \
	"Missing GPG key (specified as " ELEKTRA_RECIPIENT_KEY ") in plugin configuration. Available key IDs are: "
#define GPG_ERROR_MISSING_KEY                                                                                                              \
	"Missing GPG key (specified as " ELEKTRA_RECIPIENT_KEY                                                                             \
	") in plugin configuration. GPG could not find any secret keys. Please generate a secret key first!"
#define GPG_ERROR_INVALID_KEY "'%s' does not identify a valid GPG private key."

/**
 * List of states for the state machine that parses the gpg key list output.
 */
enum gpgKeyListState
{
	GPG_KEYLIST_STATE_START,
	GPG_KEYLIST_STATE_FPR2,
	GPG_KEYLIST_STATE_FPR3,
	GPG_KEYLIST_STATE_COLON,
	GPG_KEYLIST_STATE_KEYID
};

/**
 * Return codes for the forked child process that starts the gpg binary.
 * List of possible errors.
 */
enum gpgCallErrorCode
{

	/** Failed to duplicate the stdin pipe */
	GPG_CALL_DUP_STDIN = 0x4200,

	/** Failed to duplicate the stdout pipe */
	GPG_CALL_DUP_STDOUT = 0x4201,

	/** Failed to dupliate the stderr pipe */
	GPG_CALL_DUP_STDERR = 0x4202,

	/** Failed to execv the gpg binary */
	GPG_CALL_EXECV = 0x4203
};

struct gpgKeyListElement
{
	size_t start;
	size_t end;
	struct gpgKeyListElement * next;
};

static inline void closePipe (int * pipe)
{
	close (pipe[0]);
	close (pipe[1]);
}

/**
 * @brief checks whether or not a given file exists and is executable.
 * @param file holds the path to the file that should be checked
 * @param errorKey holds an error description if the file does not exist or if it is not executable. Ignored if set to NULL.
 * @retval 1 if the file exists and is executable
 * @retval -1 if the file can not be found
 * @retval -2 if the file exsits but it can not be executed
 */
static int isExecutable (const char * file, Key * errorKey)
{
	if (access (file, F_OK))
	{
		if (errorKey)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Gpg binary %s not found", file);
		}
		return -1;
	}

	if (access (file, X_OK))
	{
		if (errorKey)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Gpg binary %s has no permission to execute", file);
		}
		return -2;
	}

	return 1;
}

/**
 * @brief concatenates dir and file.
 * @param errorKey holds an error description in case of failure.
 * @param dir contains the path to a directory
 * @param file contains a file name
 * @returns an allocated string containing "dir:/file" which must be freed by the caller or NULL in case of error.
 */
static char * genGpgCandidate (Key * errorKey, char * dir, const char * file)
{
	const size_t resultLen = strlen (dir) + strlen (file) + 2;
	char * result = elektraMalloc (resultLen);
	if (!result)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return NULL;
	}
	snprintf (result, resultLen, "%s/%s", dir, file);
	return result;
}

/**
 * @brief lookup binary file bin in the PATH environment variable.
 * @param errorKey holds an error description in case of failure.
 * @param bin the binary file to look for
 * @param result holds an allocated string containing the full path to the binary file or NULL in case of error. Must be freed by the
 * caller.
 * @retval -1 if an error occurred. See errorKey for a description.
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
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
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
int ELEKTRA_PLUGIN_FUNCTION (gpgGetBinary) (char ** gpgBin, KeySet * conf, Key * errorKey)
{
	*gpgBin = NULL;

	// plugin configuration has highest priority
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_BIN, 0);
	if (k)
	{
		const char * configPath = keyString (k);
		const size_t configPathLen = strlen (configPath) + 1; // NULL-terminator
		if (configPathLen > 0)
		{
			*gpgBin = elektraMalloc (configPathLen + 1);
			if (!(*gpgBin))
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
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


	// last resort number one - check for gpg2 at /usr/bin/gpg2
	// NOTE this might happen if the PATH variable is empty
	if (isExecutable (ELEKTRA_CRYPTO_DEFAULT_GPG2_BIN, NULL) == 1)
	{
		*gpgBin = elektraStrDup (ELEKTRA_CRYPTO_DEFAULT_GPG2_BIN);
		if (!(*gpgBin))
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
			return -1;
		}
		return 1;
	}

	// last last resort - check for /usr/bin/gpg
	if (isExecutable (ELEKTRA_CRYPTO_DEFAULT_GPG1_BIN, NULL) == 1)
	{
		*gpgBin = elektraStrDup (ELEKTRA_CRYPTO_DEFAULT_GPG1_BIN);
		if (!(*gpgBin))
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
			return -1;
		}
		return 1;
	}

	// no GPG for us :-(
	ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "No gpg binary found. Please make sure GnuPG is installed and executable");
	return -1;
}

/**
 * @brief frees a linked list of key ids
 * @param head holds the pointer to the head of the linked list.
 */
static void freeKeyList (struct gpgKeyListElement * head)
{
	struct gpgKeyListElement * e;
	while (head)
	{
		e = head;
		head = head->next;
		elektraFree (e);
	}
}

/**
 * @brief parses the key IDs from the GPG output and writes it to gpgMissingKeyErrorBuffer.
 * @param msgKey holds the output of the GPG process
 * @param totalChars is set to the total number of characters of all key IDs read from the GPG output
 * @param keyCount is set to the numer of key Ids parsed from the GPG output
 * @returns a newly allocated linked list of key IDs. Must be freed by the caller!
 *
 * The parsing process is expressed as a state machine:
 *
 * +-------+-------------+------------+----------------------------+
 * | State |  Input      | Next State |           Action           |
 * +-------+-------------+------------+----------------------------+
 * | START | 'f'         | FPR2       |                            |
 * | START | *           | START      |                            |
 * | ----- | ----------- | ---------- | -------------------------- |
 * | FPR2  | 'p'         | FPR3       |                            |
 * | FPR2  | *           | START      |                            |
 * | ----- | ----------- | ---------- | -------------------------- |
 * | FPR3  | 'r'         | COLON      |                            |
 * | FPR3  | *           | START      |                            |
 * | ----- | ----------- | ---------- | -------------------------- |
 * | COLON | ':'         | COLON      |                            |
 * | COLON | [A-Za-z0-9] | KEYID      | start new key ID           |
 * | COLON | *           | START      |                            |
 * | ----- | ----------- | ---------- | -------------------------- |
 * | KEYID | [A-Za-z0-9] | KEYID      | save key ID                |
 * | KEYID | *           | START      | end key ID, append to list |
 * +-------+-------------+------------+----------------------------+
 *
 */
static struct gpgKeyListElement * parseGpgKeyIdFromOutput (Key * msgKey, size_t * totalChars, size_t * keyCount)
{
	// generate a list of secret key IDs
	const char * input = (char *) keyValue (msgKey);
	const ssize_t inputLen = keyGetValueSize (msgKey);
	*totalChars = 0;
	*keyCount = 0;
	struct gpgKeyListElement * keylistHead = NULL;
	enum gpgKeyListState state = GPG_KEYLIST_STATE_START;

	for (int i = 0; i < inputLen; i++)
	{
		switch (state)
		{
		case GPG_KEYLIST_STATE_START:
			if (input[i] == 'f')
			{
				state = GPG_KEYLIST_STATE_FPR2;
			}
			break;

		case GPG_KEYLIST_STATE_FPR2:
			if (input[i] == 'p')
			{
				state = GPG_KEYLIST_STATE_FPR3;
			}
			else
			{
				state = GPG_KEYLIST_STATE_START;
			}
			break;

		case GPG_KEYLIST_STATE_FPR3:
			if (input[i] == 'r')
			{
				state = GPG_KEYLIST_STATE_COLON;
			}
			else
			{
				state = GPG_KEYLIST_STATE_START;
			}
			break;

		case GPG_KEYLIST_STATE_COLON:
			if (input[i] == ':')
			{
				continue;
			}
			else if ((input[i] >= 'A' && input[i] <= 'Z') || (input[i] >= 'a' && input[i] <= 'z') ||
				 (input[i] >= '0' && input[i] <= '9'))
			{
				state = GPG_KEYLIST_STATE_KEYID;
				// start new key id entry
				if (keylistHead)
				{
					struct gpgKeyListElement * elem = elektraMalloc (sizeof (struct gpgKeyListElement));
					if (!elem)
					{
						freeKeyList (keylistHead);
						*totalChars = 0;
						*keyCount = 0;
						return NULL;
					}
					elem->start = i;
					elem->next = keylistHead;
					keylistHead = elem;
				}
				else
				{
					keylistHead = elektraMalloc (sizeof (struct gpgKeyListElement));
					if (!keylistHead)
					{
						return NULL;
					}
					keylistHead->start = i;
					keylistHead->next = NULL;
				}
				*keyCount += 1;
			}
			else
			{
				state = GPG_KEYLIST_STATE_START;
			}
			break;

		case GPG_KEYLIST_STATE_KEYID:
			if ((input[i] < 'A' || input[i] > 'Z') && (input[i] < 'a' || input[i] > 'z') && (input[i] < '0' || input[i] > '9'))
			{
				keylistHead->end = i;
				*totalChars += keylistHead->end - keylistHead->start;
				state = GPG_KEYLIST_STATE_START;
			}
			break;

		default:
			state = GPG_KEYLIST_STATE_START;
			break;
		}
	}
	return keylistHead;
}

/**
 * @brief verifies if given value is a valid GPG private key.
 * @param conf holds the plugin configuration
 * @param value to be checked
 * @retval 1 if value is a valid GPG private key
 * @retval -1 otherwise
 */
static int isValidGpgKey (KeySet * conf, const char * value)
{
	// NOTE it is save to discard the const modifier (although it is not pretty) - the value is not being modified
	char * argv[] = { "", "--batch", "--with-colons", "--fixed-list-mode", "--list-secret-keys", (char *) value, NULL };
	Key * errorKey = keyNew ("/", KEY_END);
	Key * msgKey = keyNew ("/", KEY_END);

	int status = ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msgKey, argv, 7);

	keyDel (msgKey);
	keyDel (errorKey);

	return status;
}

/**
 * @brief check all keys in conf at and under given root key, whether or not they hold an valid GPG private key identifier.
 * @param root the root of the configuration key to look for
 * @param conf holds the plugin configuration
 * @param errorKey holds error information in case of failure.
 * @retval 1 on success.
 * @retval -1 on error, check errorKey for further details.
 */
static int verifyGpgKeysInConf (Key * root, KeySet * conf, Key * errorKey)
{
	if (!root) return 1; // success

	// verify top level key elements
	const char * rootValue = keyString (root);
	if (strlen (rootValue) > 0)
	{
		if (isValidGpgKey (conf, rootValue) != 1)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, GPG_ERROR_INVALID_KEY, rootValue);
			return -1; // failure
		}
	}

	// verify child elements
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		Key * k = ksAtCursor (conf, it);
		if (keyIsBelow (k, root))
		{
			const char * childValue = keyString (k);
			if (isValidGpgKey (conf, childValue) != 1)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, GPG_ERROR_INVALID_KEY, childValue);
				return -1; // failure
			}
		}
	}

	return 1; // success
}

/**
 * @brief verifies that the config only holds valid GPG private key identifiers for encryption or signing.
 * However, this method does NOT verify that the config does contain any GPG keys at all.
 *
 * @param conf holds the plugin configuration
 * @param errorKey holds an error description in case of failure.
 * @retval 1 on success
 * @retval -1 on error. check errorKey for further details.
 */
int ELEKTRA_PLUGIN_FUNCTION (gpgVerifyGpgKeysInConfig) (KeySet * conf, Key * errorKey)
{
	Key * rootEncrypting = ksLookupByName (conf, ELEKTRA_RECIPIENT_KEY, 0);
	if (verifyGpgKeysInConf (rootEncrypting, conf, errorKey) != 1)
	{
		// errorKey has been set by verifyGpgKeysInConf
		return -1; // failure
	}

	Key * rootSignature = ksLookupByName (conf, ELEKTRA_SIGNATURE_KEY, 0);
	if (verifyGpgKeysInConf (rootSignature, conf, errorKey) != 1)
	{
		// errorKey has been set by verifyGpgKeysInConf
		return -1; // failure
	}

	return 1; // success
}

/**
 * @brief prepare the error text in case of missing GPG recipient specification in the configuration.
 * @param conf holds the backend/plugin configuration
 * @returns the error text. This pointer must be freed by the caller!
 */
char * ELEKTRA_PLUGIN_FUNCTION (getMissingGpgKeyErrorText) (KeySet * conf)
{
	Key * msgKey = keyNew ("/", KEY_END);
	Key * errorKey = keyNew ("/", KEY_END);

	char * errorBuffer;
	size_t errorBufferLen = 0;

	keySetBinary (msgKey, NULL, 0);
	char * argv[] = { "", "--batch", "--list-secret-keys", "--with-fingerprint", "--with-colons", "--fixed-list-mode", NULL };
	if (ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msgKey, argv, 7) == 1)
	{
		size_t totalKeyIdChars = 0;
		size_t keyCount = 0;
		struct gpgKeyListElement * listHead = parseGpgKeyIdFromOutput (msgKey, &totalKeyIdChars, &keyCount);

		if (keyCount > 0)
		{
			// error message + list of all key ids separated by a coma ',' + null terminator
			errorBufferLen = strlen (GPG_ERROR_MISSING_KEY_LIST) + totalKeyIdChars + keyCount;
			errorBuffer = elektraMalloc (errorBufferLen);
			if (!errorBuffer)
			{
				freeKeyList (listHead);
				return NULL;
			}

			const char * content = (const char *) keyValue (msgKey);

			size_t index = strlen (GPG_ERROR_MISSING_KEY_LIST);
			strncpy (errorBuffer, GPG_ERROR_MISSING_KEY_LIST, errorBufferLen);

			// construct the error list with the key ids
			struct gpgKeyListElement * iterator = listHead;
			while (iterator)
			{
				if (iterator != listHead)
				{
					errorBuffer[index++] = ',';
				}

				strncpy (&errorBuffer[index], &content[iterator->start], iterator->end - iterator->start);
				index += iterator->end - iterator->start;
				iterator = iterator->next;
			}
			errorBuffer[index] = '\0';

			freeKeyList (listHead);
			keyDel (msgKey);
			keyDel (errorKey);
			return errorBuffer;
		}
	}

	// default message - we could not find a GPG secret key
	errorBufferLen = strlen (GPG_ERROR_MISSING_KEY) + 1;
	errorBuffer = elektraMalloc (errorBufferLen);
	if (errorBuffer)
	{
		strncpy (errorBuffer, GPG_ERROR_MISSING_KEY, errorBufferLen);
	}
	keyDel (msgKey);
	keyDel (errorKey);
	return errorBuffer;
}

/**
 * @brief call the gpg binary to perform the requested operation.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the message to be transformed. Ignored if set to NULL, i.e. you do not want to send a stdin message to gpg.
 * @param argv array holds the arguments passed on to the gpg process
 * @param argc contains the number of elements in argv, i.e. the size of argv
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (gpgCall) (KeySet * conf, Key * errorKey, Key * msgKey, char * argv[], size_t argc)
{
	pid_t pid;
	int status;
	int pipe_stdin[2];
	int pipe_stdout[2];
	int pipe_stderr[2];
	char errorBuffer[512] = "";
	kdb_octet_t * buffer = NULL;
	ssize_t bufferSize = 2 * keyGetValueSize (msgKey);
	ssize_t outputLen;

	assert (argc > 1);

	// check if bufferSize is valid
	if (bufferSize <= 0)
	{
		bufferSize = GPG_OUTPUT_DEFAULT_BUFFER_SIZE;
	}

	// sanitize the argument vector
	if (ELEKTRA_PLUGIN_FUNCTION (gpgGetBinary) (&argv[0], conf, errorKey) != 1)
	{
		return -1;
	}
	argv[argc - 1] = NULL;

	// check that the gpg binary exists and that it is executable
	if (isExecutable (argv[0], errorKey) != 1)
	{
		elektraFree (argv[0]);
		return -1; // error set by isExecutable()
	}

	// initialize pipes
	if (pipe (pipe_stdin))
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Pipe initialization failed");
		elektraFree (argv[0]);
		return -1;
	}

	if (pipe (pipe_stdout))
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Pipe initialization failed");
		closePipe (pipe_stdin);
		elektraFree (argv[0]);
		return -1;
	}

	if (pipe (pipe_stderr))
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Pipe initialization failed");
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		elektraFree (argv[0]);
		return -1;
	}

	// allocate buffer for gpg output
	// estimated maximum output size = 2 * input (including headers, etc.)
	if (msgKey && !(buffer = elektraMalloc (bufferSize)))
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		closePipe (pipe_stderr);
		elektraFree (argv[0]);
		return -1;
	}

	// fork into the gpg binary
	switch (pid = fork ())
	{
	case -1:
		// fork() failed
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Fork failed. Reason: %s", strerror (errno));
		closePipe (pipe_stdin);
		closePipe (pipe_stdout);
		closePipe (pipe_stderr);
		elektraFree (buffer);
		elektraFree (argv[0]);
		return -1;

	case 0:
		// start of the forked child process
		close (pipe_stdin[1]);
		close (pipe_stdout[0]);
		close (pipe_stderr[0]);

		// redirect stdin to pipe
		if (msgKey)
		{
			close (STDIN_FILENO);
			if (dup (pipe_stdin[0]) < 0)
			{
				exit (GPG_CALL_DUP_STDIN);
			}
		}
		close (pipe_stdin[0]);

		// redirect stdout to pipe
		close (STDOUT_FILENO);
		if (dup (pipe_stdout[1]) < 0)
		{
			exit (GPG_CALL_DUP_STDOUT);
		}
		close (pipe_stdout[1]);

		// redirect stderr to pipe
		close (STDERR_FILENO);
		if (dup (pipe_stderr[1]) < 0)
		{
			exit (GPG_CALL_DUP_STDERR);
		}
		close (pipe_stderr[1]);

		// finally call the gpg executable
		if (execv (argv[0], argv) < 0)
		{
			// errno is set according to the man page of execv
			fprintf (stderr, "%d", errno);
			exit (GPG_CALL_EXECV);
		}
		// end of the child process
	}

	// parent process
	close (pipe_stdin[0]);
	close (pipe_stdout[1]);
	close (pipe_stderr[1]);

	// pass the message to the gpg process
	const ssize_t sendMessageSize = keyGetValueSize (msgKey);
	if (msgKey && sendMessageSize > 0)
	{
		if (write (pipe_stdin[1], keyValue (msgKey), sendMessageSize) != sendMessageSize)
		{
			ELEKTRA_SET_RESOURCE_ERROR (errorKey, "The communication with the GPG process failed");
			closePipe (pipe_stdin);
			closePipe (pipe_stdout);
			closePipe (pipe_stderr);
			elektraFree (buffer);
			elektraFree (argv[0]);
			return -1;
		}
	}
	close (pipe_stdin[1]);

	// wait for the gpg process to finish
	waitpid (pid, &status, 0);

	// evaluate return code of finished child process
	int retval = -1;
	switch (status)
	{
	case 0:
		// everything ok - receive the output of the gpg process
		if (msgKey)
		{
			outputLen = read (pipe_stdout[0], buffer, bufferSize);
			keySetBinary (msgKey, buffer, outputLen);
		}
		retval = 1;
		break;

	case 1:
		// bad signature
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "GPG reported a bad signature. Reason: %s", strerror (errno));
		break;

	case GPG_CALL_DUP_STDIN:
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Failed to redirect stdin");
		break;

	case GPG_CALL_DUP_STDOUT:
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Failed to redirect stdout");
		break;

	case GPG_CALL_DUP_STDERR:
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Failed to redirect stderr");
		break;

	case GPG_CALL_EXECV:
		outputLen = read (pipe_stderr[0], errorBuffer, sizeof (errorBuffer));
		if (outputLen < 1)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Failed to start the gpg binary \"%s\"", argv[0]);
		}
		else
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Failed to start the gpg binary \"%s\", reason: %s", argv[0],
							 strerror (atoi (errorBuffer)));
		}
		break;

	default:
		// other errors
		outputLen = read (pipe_stderr[0], errorBuffer, sizeof (errorBuffer));
		if (outputLen < 1)
		{
			errorBuffer[0] = '\0';
		}
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "GPG failed with return value %d. %s", status, errorBuffer);
		break;
	}

	elektraFree (buffer);
	elektraFree (argv[0]);
	close (pipe_stdout[0]);
	close (pipe_stderr[0]);
	return retval;
}
