/**
 * @file
 *
 * @brief Source for process plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./process.h"

#include <elektra/core/errors.h>
#include <elektra/plugin/invoke.h>

#include <internal/core/keyset/api.h>
#include <internal/kdbprivate.h>
#include <internal/utility/alloc.h>
#include <internal/utility/array.h>
#include <internal/utility/compare.h>
#include <internal/utility/format.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

typedef struct
{
	pid_t childPid;
	FILE * toChild;
	FILE * fromChild;
	Key * childContractKey;
	KeySet * childContract;
	ElektraInvokeHandle * dump;
	struct
	{
		bool open;
		bool get;
		bool set;
		bool close;
	} hasOp;
} IoData;

#define MSG_HANDSHAKE_HEADER_V1 "ELEKTRA_PROCESS INIT v1\n"
#define MSG_HANDSHAKE_ACK_V1 "ELEKTRA_PROCESS ACK v1\n"
#define MSG_TERMINATION "ELEKTRA_PROCESS TERMINATE\n"


static char ** readArgs (KeySet * config, const char * arg0);
static char ** readEnv (KeySet * config);
static void freeConfigArray (char ** array);
static int executeOperation (IoData * data, const char * op, KeySet * ks, bool readKs, Key * parentKey);
static KeySet * readKeySet (IoData * data, Key * errorKey);
static void deleteData (IoData * data, Key * errorKey);

int elektraProcessOpen (Plugin * handle, Key * errorKey)
{
	KeySet * config = elektraPluginGetConfig (handle);

	if (ksLookupByName (config, "system:/module", 0) != NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * appKey = ksLookupByName (config, "/executable", 0);
	const char * appPath = appKey == NULL ? NULL : keyString (appKey);

	if (appKey == NULL || appPath == NULL)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "The /executable config key is missing");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (appPath[0] != '/')
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "The value of the /executable config key is not an absolute path: '%s'",
							 appPath);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char ** args = readArgs (config, appPath);
	char ** env = readEnv (config);

	Key * appConfigRoot = keyNew ("/config", KEY_END);
	KeySet * appConfig = ksCut (config, appConfigRoot);
	keyDel (appConfigRoot);
	ksAppend (config, appConfig);

	pid_t pid;
	int parentToChild[2], childToParent[2];

	if (pipe (parentToChild) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (Couldn't open pipe p2c). Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (pipe (childToParent) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (Couldn't open pipe c2p). Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	pid = fork ();

	if (pid == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app '%s' (Fork failed). Reason: %s", appPath, strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (pid == 0)
	{
		// child
		if (dup2 (parentToChild[0], STDIN_FILENO) == -1)
		{
			exit (EXIT_FAILURE);
		}

		close (parentToChild[0]);
		close (parentToChild[1]);

		if (dup2 (childToParent[1], STDOUT_FILENO) == -1)
		{
			exit (EXIT_FAILURE);
		}

		close (childToParent[0]);
		close (childToParent[1]);

		execve (appPath, args, env);
		perror (appPath);
		exit (EXIT_FAILURE);
	}

	// parent

	freeConfigArray (args);
	freeConfigArray (env);

	close (parentToChild[0]);
	close (childToParent[1]);

	if (write (parentToChild[1], MSG_HANDSHAKE_HEADER_V1, sizeof (MSG_HANDSHAKE_HEADER_V1) - 1) != sizeof (MSG_HANDSHAKE_HEADER_V1) - 1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (handshake write failed). Reason: %s", strerror (errno));
		close (parentToChild[1]);
		close (childToParent[0]);
		kill (pid, SIGTERM);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char handshakeAck[sizeof (MSG_HANDSHAKE_ACK_V1)];
	ssize_t readBytes = read (childToParent[0], handshakeAck, sizeof (MSG_HANDSHAKE_ACK_V1) - 1);
	if (readBytes != sizeof (MSG_HANDSHAKE_ACK_V1) - 1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (handshake read failed). Reason: %s", strerror (errno));
		close (parentToChild[1]);
		close (childToParent[0]);
		kill (pid, SIGTERM);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	handshakeAck[sizeof (MSG_HANDSHAKE_ACK_V1) - 1] = '\0';

	if (strcmp (handshakeAck, MSG_HANDSHAKE_ACK_V1) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not execute app (handshake ack failed). Reason: broken ack");
		close (parentToChild[1]);
		close (childToParent[0]);
		kill (pid, SIGTERM);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	FILE * toChild = fdopen (parentToChild[1], "wb");
	FILE * fromChild = fdopen (childToParent[0], "rb");

	IoData * data = elektraCalloc (sizeof (IoData));

	data->childPid = pid;
	data->toChild = toChild;
	data->fromChild = fromChild;

	data->dump = elektraInvokeOpen ("dump", NULL, errorKey);
	if (data->dump == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not execute app (dump init failed).");
		deleteData (data, errorKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char * childName = NULL;
	size_t n = 0;
	readBytes = getline (&childName, &n, fromChild);
	if (readBytes < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (name read failed). Reason: %s", strerror (errno));
		deleteData (data, errorKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	childName[readBytes - 1] = '\0';

	KeySet * contract = readKeySet (data, errorKey);
	if (contract == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not execute app (contract read failed)");
		deleteData (data, errorKey);
		free (childName);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * openKey = ksLookupByName (contract, "system:/elektra/modules/process/exports/has/open", KDB_O_POP);
	if (openKey != NULL && strcmp (keyString (openKey), "1") == 0)
	{
		data->hasOp.open = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/process/exports/open", KEY_FUNC, elektraProcessOpen, KEY_END));
	}
	else
	{
		data->hasOp.open = false;
	}
	keyDel (openKey);

	Key * getKey = ksLookupByName (contract, "system:/elektra/modules/process/exports/has/get", KDB_O_POP);
	if (getKey != NULL && strcmp (keyString (getKey), "1") == 0)
	{
		data->hasOp.get = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/process/exports/get", KEY_FUNC, elektraProcessGet, KEY_END));
	}
	else
	{
		data->hasOp.get = false;
	}
	keyDel (getKey);

	Key * setKey = ksLookupByName (contract, "system:/elektra/modules/process/exports/has/set", KDB_O_POP);
	if (setKey != NULL && strcmp (keyString (setKey), "1") == 0)
	{
		data->hasOp.set = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/process/exports/set", KEY_FUNC, elektraProcessSet, KEY_END));
	}
	else
	{
		data->hasOp.set = false;
	}
	keyDel (setKey);

	Key * closeKey = ksLookupByName (contract, "system:/elektra/modules/process/exports/has/close", KDB_O_POP);
	if (closeKey != NULL && strcmp (keyString (closeKey), "1") == 0)
	{
		data->hasOp.close = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/process/exports/close", KEY_FUNC, elektraProcessClose, KEY_END));
	}
	else
	{
		data->hasOp.close = false;
	}
	keyDel (closeKey);

	Key * oldContractRoot = keyNew ("system:/elektra/modules/process", KEY_END);
	Key * newContractRoot = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (newContractRoot, childName);
	free (childName);

	data->childContractKey = newContractRoot;
	data->childContract = contract;
	ksRename (data->childContract, oldContractRoot, newContractRoot);

	keyDel (oldContractRoot);

	elektraPluginSetData (handle, data);

	int result;
	if (data->hasOp.open)
	{
		result = executeOperation (data, "open", appConfig, false, errorKey);
	}
	else
	{
		result = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	ksDel (appConfig);
	return result;
}

int elektraProcessClose (Plugin * handle, Key * errorKey)
{
	IoData * data = elektraPluginGetData (handle);
	if (data == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * eKey = errorKey == NULL ? keyNew ("/", KEY_END) : errorKey;

	int result;
	if (data->hasOp.close)
	{
		result = executeOperation (data, "close", NULL, false, eKey);
	}
	else
	{
		result = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	bool error = false;
	if (data->toChild != NULL && fputs (MSG_TERMINATION, data->toChild) == EOF)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (eKey, "Could not terminate app (write failed). Reason: %s", strerror (errno));
		error = true;
	}
	fflush (data->toChild);

	pid_t childPid = data->childPid;
	elektraPluginSetData (handle, NULL);
	deleteData (data, eKey);

	if (childPid != 0)
	{
		kill (childPid, SIGTERM);
		int status;
		do
		{
			pid_t w = waitpid (childPid, &status, 0);
			if (w == -1)
			{
				ELEKTRA_SET_RESOURCE_ERRORF (eKey, "Could not terminate app (waitpid). Reason: %s", strerror (errno));
			}
		} while (!WIFEXITED (status) && !WIFSIGNALED (status));
	}

	if (errorKey == NULL)
	{
		keyDel (eKey);
	}

	return error ? ELEKTRA_PLUGIN_STATUS_ERROR : result;
}

int elektraProcessGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/process"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/process", KEY_VALUE, "process plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/process/exports", KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/open", KEY_FUNC, elektraProcessOpen, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/close", KEY_FUNC, elektraProcessClose, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/get", KEY_FUNC, elektraProcessGet, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/set", KEY_FUNC, elektraProcessSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/process/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	IoData * data = elektraPluginGetData (handle);
	if (!elektraStrCmp (keyName (parentKey), keyName (data->childContractKey)))
	{
		ksAppend (returned, data->childContract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (data->hasOp.get)
	{
		return executeOperation (data, "get", returned, true, parentKey);
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

int elektraProcessSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	IoData * data = elektraPluginGetData (handle);
	if (data->hasOp.set)
	{
		return executeOperation (data, "set", returned, true, parentKey);
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("process",
		ELEKTRA_PLUGIN_OPEN,	&elektraProcessOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraProcessClose,
		ELEKTRA_PLUGIN_GET,	&elektraProcessGet,
		ELEKTRA_PLUGIN_SET,	&elektraProcessSet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}

// private functions

static char ** readArgs (KeySet * config, const char * arg0)
{
	Key * root = ksLookupByName (config, "/args", 0);
	const char * lastIndex = root == NULL ? NULL : keyString (root);

	if (lastIndex == NULL || strlen (lastIndex) == 0)
	{
		char ** array = elektraMalloc (sizeof (char *) * 2);
		array[0] = strdup (arg0);
		array[1] = NULL;
		return array;
	}

	char index[ELEKTRA_MAX_ARRAY_SIZE] = "";
	kdb_long_long_t count = 0;
	while (strcmp (index, lastIndex) <= 0)
	{
		count++;
		elektraWriteArrayNumber (index, count);
	}


	char ** array = elektraMalloc (sizeof (char *) * (count + 2));
	array[0] = strdup (arg0);
	array[count + 1] = NULL;

	Key * lookup = keyDup (root, KEY_CP_NAME);
	keySetNamespace (lookup, KEY_NS_CASCADING);
	keyAddBaseName (lookup, "");
	for (kdb_long_long_t i = 0; i < count; i++)
	{
		elektraWriteArrayNumber (index, i);
		keySetBaseName (lookup, index);

		Key * key = ksLookup (config, lookup, 0);
		array[i + 1] = strdup (keyString (key));
	}
	keyDel (lookup);

	return array;
}

static char ** readEnv (KeySet * config)
{
	char index[ELEKTRA_MAX_ARRAY_SIZE];

	Key * envRoot = ksLookupByName (config, "/env", 0);
	const char * envLastIndex = envRoot == NULL ? NULL : keyString (envRoot);

	kdb_long_long_t envCount = 0;
	if (envLastIndex != NULL && strlen (envLastIndex) > 0)
	{
		index[0] = '\0';
		while (strcmp (index, envLastIndex) <= 0)
		{
			envCount++;
			elektraWriteArrayNumber (index, envCount);
		}
	}


	Key * copyRoot = ksLookupByName (config, "/copyenv", 0);
	const char * copyLastIndex = copyRoot == NULL ? NULL : keyString (copyRoot);

	kdb_long_long_t copyCount = 0;
	if (copyLastIndex != NULL && strlen (copyLastIndex) > 0)
	{
		index[0] = '\0';
		while (strcmp (index, copyLastIndex) <= 0)
		{
			copyCount++;
			elektraWriteArrayNumber (index, copyCount);
		}
	}

	if (copyCount == 0 && envCount == 0)
	{
		return elektraCalloc (sizeof (char *));
	}


	char ** array = elektraMalloc (sizeof (char *) * (envCount + copyCount + 1));

	size_t arrayPos = 0;

	Key * lookup = keyDup (envRoot, KEY_CP_NAME);
	keySetNamespace (lookup, KEY_NS_CASCADING);
	keyAddBaseName (lookup, "");
	for (kdb_long_long_t i = 0; i < envCount; i++)
	{
		elektraWriteArrayNumber (index, i);
		keySetBaseName (lookup, index);

		Key * key = ksLookup (config, lookup, 0);
		array[arrayPos++] = strdup (keyString (key));
	}

	keyCopy (lookup, copyRoot, KEY_CP_NAME);
	keySetNamespace (lookup, KEY_NS_CASCADING);
	keyAddBaseName (lookup, "");
	for (kdb_long_long_t i = 0; i < copyCount; i++)
	{
		elektraWriteArrayNumber (index, i);
		keySetBaseName (lookup, index);

		Key * key = ksLookup (config, lookup, 0);
		const char * envValue = getenv (keyString (key));
		if (envValue != NULL)
		{
			char * envVar = elektraFormat ("%s=%s", keyString (key), envValue);
			array[arrayPos++] = strdup (envVar);
			elektraFree (envVar);
		}
	}
	array[arrayPos] = NULL;

	keyDel (lookup);

	return array;
}

static void freeConfigArray (char ** array)
{
	for (size_t i = 0; array[i] != NULL; i++)
	{
		// comes from strdup so we use free directly
		free (array[i]);
	}

	elektraFree (array);
}

static void deleteData (IoData * data, Key * errorKey)
{
	// TODO: errors
	data->childPid = 0;
	elektraInvokeClose (data->dump, errorKey);
	ksDel (data->childContract);
	keyDel (data->childContractKey);
	fclose (data->fromChild);
	fclose (data->toChild);
	elektraFree (data);
}

static int executeOperation (IoData * data, const char * op, KeySet * ks, bool readKs, Key * parentKey)
{
	typedef int (*fserialize_t) (KeySet *, FILE *, Key *);

	fserialize_t fserialize = *(fserialize_t *) elektraInvokeGetFunction (data->dump, "fserialize");

	if (fserialize == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Could not execute  '%s' (write failed). Reason: fserialize missing", op);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (fprintf (data->toChild, "%s\n", op) < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute operation '%s' (write failed). Reason: %s", op,
					     strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	KeySet * parentKs = ksNew (1, keyDup (parentKey, KEY_CP_ALL), KS_END);
	if (fserialize (parentKs, data->toChild, parentKey) < 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ksDel (parentKs);

	if (ks != NULL)
	{
		if (fserialize (ks, data->toChild, parentKey) < 0)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	fflush (data->toChild);

	char * result = NULL;
	size_t n = 0;
	ssize_t readBytes = getline (&result, &n, data->fromChild);
	if (readBytes < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute operation '%s' (read failed). Reason: %s", op, strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	result[readBytes - 1] = '\0';

	KeySet * newParentKs = readKeySet (data, parentKey);
	if (newParentKs == NULL || ksGetSize (newParentKs) != 1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute operation '%s'. Reason: funserialize failed", op);
		free (result);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// Note: value might be read-only, so copy value and meta separately to ensure we copy as much as possible
	keyCopy (parentKey, ksAtCursor (newParentKs, 0), KEY_CP_VALUE);
	keyCopy (parentKey, ksAtCursor (newParentKs, 0), KEY_CP_META);
	ksDel (newParentKs);

	if (ks != NULL && readKs)
	{
		KeySet * returned = readKeySet (data, parentKey);
		if (returned == NULL)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute operation '%s'. Reason: funserialize failed", op);
			free (result);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		ksClear (ks);
		ksAppend (ks, returned);
		ksDel (returned);
	}

	int rc;
	if (strcmp (result, "success") == 0)
	{
		rc = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (result, "noupdate") == 0)
	{
		rc = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else if (strcmp (result, "error") == 0)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Process returned error result without setting meta:/error on parent key.");
		}
		rc = ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app (read failed). Reason: unknown result '%s'", result);
		rc = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	free (result);
	return rc;
}

static KeySet * readKeySet (IoData * data, Key * errorKey)
{
	typedef int (*funserialize_t) (KeySet *, FILE *, Key *);

	funserialize_t funserialize = *(funserialize_t *) elektraInvokeGetFunction (data->dump, "funserialize");

	if (funserialize == NULL)
	{
		return NULL;
	}

	KeySet * ks = ksNew (0, KS_END);
	if (funserialize (ks, data->fromChild, errorKey) < 0)
	{
		return NULL;
	}
	return ks;
}
