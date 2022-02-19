/**
 * @file
 *
 * @brief Source for stdioproc plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "stdioproc.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbprivate.h>

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

#define MSG_HANDSHAKE_HEADER_V1 "ELEKTRA_STDIOPROC INIT v1\n"
#define MSG_HANDSHAKE_ACK_V1 "ELEKTRA_STDIOPROC ACK v1\n"
#define MSG_TERMINATION "ELEKTRA_STDIOPROC TERMINATE\n"


static char ** readConfigArray (KeySet * config, const char * name, const char * firstElement);
static int executeOperation (IoData * data, const char * op, KeySet * ks, bool readKs, Key * parentKey);
static KeySet * readKeySet (IoData * data, Key * errorKey);
static void deleteData (IoData * data, Key * errorKey);

int elektraStdioprocOpen (Plugin * handle, Key * errorKey)
{
	KeySet * config = elektraPluginGetConfig (handle);

	Key * appKey = ksLookupByName (config, "/app", 0);
	const char * appPath = appKey == NULL ? NULL : keyString (appKey);

	if (appKey == NULL || appPath == NULL)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "The /app config key is missing");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (appPath[0] != '/')
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "The value of the /app config key is not an absolute path: '%s'",
							 appPath);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char ** args = readConfigArray (config, "/args", appPath);
	char ** env = readConfigArray (config, "/env", NULL);

	Key * appConfigRoot = keyNew ("/config", KEY_END);
	KeySet * appConfig = ksCut (config, appConfigRoot);
	keyDel (appConfigRoot);
	ksAppend (config, appConfig);

	pid_t pid;
	int parentToChild[2], childToParent[2];

	if (pipe (parentToChild) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app. Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (pipe (childToParent) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app. Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	pid = fork ();

	if (pid == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app. Reason: %s", strerror (errno));
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

	close (parentToChild[0]);
	close (childToParent[1]);

	if (write (parentToChild[1], MSG_HANDSHAKE_HEADER_V1, sizeof (MSG_HANDSHAKE_HEADER_V1) - 1) != sizeof (MSG_HANDSHAKE_HEADER_V1) - 1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (handshake failed). Reason: %s", strerror (errno));
		close (parentToChild[1]);
		close (childToParent[0]);
		kill (pid, SIGTERM);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char handshakeAck[sizeof (MSG_HANDSHAKE_ACK_V1)];
	ssize_t readBytes = read (childToParent[0], handshakeAck, sizeof (MSG_HANDSHAKE_ACK_V1) - 1);
	if (readBytes != sizeof (MSG_HANDSHAKE_ACK_V1) - 1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (handshake failed). Reason: %s", strerror (errno));
		close (parentToChild[1]);
		close (childToParent[0]);
		kill (pid, SIGTERM);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	handshakeAck[sizeof (MSG_HANDSHAKE_ACK_V1) - 1] = '\0';

	if (strcmp (handshakeAck, MSG_HANDSHAKE_ACK_V1) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not execute app (handshake failed). Reason: broken ack");
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
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not execute app (init failed). Reason: %s", strerror (errno));
		deleteData (data, errorKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	childName[readBytes - 1] = '\0';

	KeySet * contract = readKeySet (data, errorKey);

	Key * openKey = ksLookupByName (contract, "system:/elektra/modules/stdioproc/exports/open", KDB_O_POP);
	if (openKey != NULL && strcmp (keyString (openKey), "1") == 0)
	{
		data->hasOp.open = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/stdioproc/exports/open", KEY_FUNC, elektraStdioprocOpen, KEY_END));
	}
	else
	{
		data->hasOp.open = false;
	}
	keyDel (openKey);

	Key * getKey = ksLookupByName (contract, "system:/elektra/modules/stdioproc/exports/get", KDB_O_POP);
	if (getKey != NULL && strcmp (keyString (getKey), "1") == 0)
	{
		data->hasOp.get = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/stdioproc/exports/get", KEY_FUNC, elektraStdioprocGet, KEY_END));
	}
	else
	{
		data->hasOp.get = false;
	}
	keyDel (getKey);

	Key * setKey = ksLookupByName (contract, "system:/elektra/modules/stdioproc/exports/set", KDB_O_POP);
	if (setKey != NULL && strcmp (keyString (setKey), "1") == 0)
	{
		data->hasOp.set = true;
		ksAppendKey (contract, keyNew ("system:/elektra/modules/stdioproc/exports/set", KEY_FUNC, elektraStdioprocSet, KEY_END));
	}
	else
	{
		data->hasOp.set = false;
	}
	keyDel (setKey);

	Key * closeKey = ksLookupByName (contract, "system:/elektra/modules/stdioproc/exports/close", KDB_O_POP);
	if (closeKey != NULL && strcmp (keyString (closeKey), "1") == 0)
	{
		data->hasOp.close = true;
		ksAppendKey (contract,
			     keyNew ("system:/elektra/modules/stdioproc/exports/close", KEY_FUNC, elektraStdioprocClose, KEY_END));
	}
	else
	{
		data->hasOp.close = false;
	}
	keyDel (closeKey);

	Key * oldContractRoot = keyNew ("system:/elektra/modules/stdioproc", KEY_END);
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

int elektraStdioprocClose (Plugin * handle, Key * errorKey)
{
	IoData * data = elektraPluginGetData (handle);
	if (data == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int result;
	if (data->hasOp.close)
	{
		result = executeOperation (data, "close", NULL, false, errorKey);
	}
	else
	{
		result = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	bool error = false;
	if (data->toChild != NULL && fputs (MSG_TERMINATION, data->toChild) == EOF)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not terminate app. Reason: %s", strerror (errno));
		error = true;
	}
	fflush (data->toChild);

	pid_t childPid = data->childPid;
	elektraPluginSetData (handle, NULL);
	deleteData (data, errorKey);

	if (childPid != 0)
	{
		kill (childPid, SIGTERM);
		int status;
		do
		{
			pid_t w = waitpid (childPid, &status, 0);
			if (w == -1)
			{
				ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not terminate app (waitpid). Reason: %s", strerror (errno));
			}
		} while (!WIFEXITED (status) && !WIFSIGNALED (status));
	}

	return error ? ELEKTRA_PLUGIN_STATUS_ERROR : result;
}

int elektraStdioprocGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/stdioproc"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/stdioproc", KEY_VALUE, "stdioproc plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports", KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/open", KEY_FUNC, elektraStdioprocOpen, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/close", KEY_FUNC, elektraStdioprocClose, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/get", KEY_FUNC, elektraStdioprocGet, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/set", KEY_FUNC, elektraStdioprocSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/stdioproc/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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

int elektraStdioprocSet (Plugin * handle, KeySet * returned, Key * parentKey)
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
	return elektraPluginExport ("stdioproc",
		ELEKTRA_PLUGIN_OPEN,	&elektraStdioprocOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraStdioprocClose,
		ELEKTRA_PLUGIN_GET,	&elektraStdioprocGet,
		ELEKTRA_PLUGIN_SET,	&elektraStdioprocSet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}

// private functions

static char ** readConfigArray (KeySet * config, const char * name, const char * firstElement)
{
	Key * root = ksLookupByName (config, name, 0);
	const char * lastIndex = root == NULL ? NULL : keyString (root);

	if (lastIndex == NULL)
	{
		if (firstElement != NULL)
		{
			char ** array = elektraMalloc (sizeof (char *) * 2);
			array[0] = strdup (firstElement);
			array[1] = NULL;
			return array;
		}
		else
		{
			return elektraCalloc (sizeof (char *));
		}
	}

	char index[ELEKTRA_MAX_ARRAY_SIZE] = "";
	kdb_long_long_t count = 0;
	while (strcmp (index, lastIndex) <= 0)
	{
		count++;
		elektraWriteArrayNumber (index, count);
	}

	char ** array;
	size_t first;
	if (firstElement == NULL)
	{
		array = elektraMalloc (sizeof (char *) * (count + 1));
		first = 0;
		array[count] = NULL;
	}
	else
	{
		array = elektraMalloc (sizeof (char *) * (count + 2));
		array[0] = strdup (firstElement);
		first = 1;
		array[count + 1] = NULL;
	}

	Key * lookup = keyDup (root, KEY_CP_NAME);
	keySetNamespace (lookup, KEY_NS_CASCADING);
	keyAddBaseName (lookup, "");
	for (kdb_long_long_t i = 0; i < count; i++)
	{
		elektraWriteArrayNumber (index, i);
		keySetBaseName (lookup, index);

		Key * key = ksLookup (config, lookup, 0);
		array[first + i] = strdup (keyString (key));
	}
	keyDel (lookup);

	return array;
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
}

static int executeOperation (IoData * data, const char * op, KeySet * ks, bool readKs, Key * parentKey)
{
	typedef int (*fwriteks_t) (KeySet *, FILE *, Key *);

	fwriteks_t fwriteks = *(fwriteks_t *) elektraInvokeGetFunction (data->dump, "fwriteks");

	if (fwriteks == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Could not execute app (write failed). Reason: fwriteks missing");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (fprintf (data->toChild, "%s\n", op) < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app (write failed). Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	KeySet * parentKs = ksNew (1, keyDup (parentKey, KEY_CP_ALL), KS_END);
	if (fwriteks (parentKs, data->toChild, parentKey) < 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ksDel (parentKs);

	if (ks != NULL)
	{
		if (fwriteks (ks, data->toChild, parentKey) < 0)
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
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app (read failed). Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	result[readBytes - 1] = '\0';

	KeySet * newParentKs = readKeySet (data, parentKey);
	if (newParentKs == NULL || ksGetSize (newParentKs) != 1)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	keyCopy (parentKey, ksAtCursor (newParentKs, 0), KEY_CP_ALL);
	ksDel (newParentKs);

	if (ks != NULL && readKs)
	{
		KeySet * returned = readKeySet (data, parentKey);
		ksClear (ks);
		ksAppend (ks, returned);
	}

	if (strcmp (result, "success") == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (result, "noupdate") == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else if (strcmp (result, "error") == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app (read failed). Reason: unknown result '%s'", result);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static KeySet * readKeySet (IoData * data, Key * errorKey)
{
	typedef int (*freadks_t) (KeySet *, FILE *, Key *);

	freadks_t freadks = *(freadks_t *) elektraInvokeGetFunction (data->dump, "freadks");

	if (freadks == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "Could not execute app (write failed). Reason: freadks missing");
		return false;
	}

	KeySet * ks = ksNew (0, KS_END);
	if (freadks (ks, data->fromChild, errorKey) < 0)
	{
		return false;
	}
	return ks;
}
