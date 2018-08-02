/**
 * @file
 *
 * @brief Source for the pluginprocess library
 *
 * Executes plugins in a separate process via fork and uses a simple
 * communication protocol based on the dump plugin via named pipes.
 *
 * Plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbpluginprocess.h"
#include <kdberrors.h>
#include <kdbinvoke.h>
#include <kdblogger.h>
#include <kdbprivate.h> // To access the plugin function pointers

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct _ElektraPluginProcess
{
	char * pipeDirectory;
	char * commandPipe;
	char * resultPipe;
	int pid;
	int counter;
	ElektraInvokeHandle * dump;
	void * pluginData;
};

static void cleanupPluginData (ElektraPluginProcess * pp, Key * errorKey)
{
	if (pp->dump) elektraInvokeClose (pp->dump, errorKey);
	if (pp->pid && pp->commandPipe) unlink (pp->commandPipe);
	elektraFree (pp->commandPipe);
	if (pp->pid && pp->resultPipe) unlink (pp->resultPipe);
	elektraFree (pp->resultPipe);
	if (pp->pid && pp->pipeDirectory) rmdir (pp->pipeDirectory);
	elektraFree (pp->pipeDirectory);
	elektraFree (pp);
}

static char * intToStr (int i)
{
	int size;
	char * str;
	size = snprintf (NULL, 0, "%d", i);
	str = elektraMalloc (size + 1);
	size = snprintf (str, size + 1, "%d", i);
	return str;
}

/** Start the child process' command loop
 *
 * This will make the child process wait for plugin commands
 * and execute them, returning the result to the parent. This
 * is typically called in a plugin's open function.
 *
 * @param handle the plugin's handle
 * @param pp the data structure containing the plugin's process information
 * @see elektraPluginProcessInit how to use this function in a plugin
 * @ingroup processplugin
 **/
void elektraPluginProcessStart (Plugin * handle, ElektraPluginProcess * pp)
{
	Key * commandPipeKey = keyNew ("/pluginprocess/pipe/command", KEY_VALUE, pp->commandPipe, KEY_END);
	Key * resultPipeKey = keyNew ("/pluginprocess/pipe/result", KEY_VALUE, pp->resultPipe, KEY_END);
	KeySet * commandKeySet = ksNew (6, KS_END);
	int counter = 0;

	do
	{
		KeySet * keySet = NULL;
		ELEKTRA_LOG_DEBUG ("Child: Wait for commands");
		elektraInvoke2Args (pp->dump, "get", commandKeySet, commandPipeKey);
		if (elektraStrCmp (keyString (ksLookupByName (commandKeySet, "/pluginprocess/payload/exists", KDB_O_NONE)), "1") == 0)
		{
			keySet = ksNew (0, KS_END);
			// We use the two pipes "interleaved" to avoid concurrency issues that can arise when sending another keyset
			// while the pipe is currently being deserialized
			elektraInvoke2Args (pp->dump, "get", keySet, resultPipeKey);
			ELEKTRA_LOG_DEBUG ("Child: We received a KeySet with %zd keys in it", ksGetSize (keySet));
		}

		Key * commandKey = ksLookupByName (commandKeySet, "/pluginprocess/command", KDB_O_NONE);
		Key * originalNameKey = ksLookupByName (commandKeySet, "/pluginprocess/original", KDB_O_NONE);
		Key * originalKey = ksLookupByName (commandKeySet, keyString (originalNameKey), KDB_O_NONE);
		int result = ELEKTRA_PLUGIN_STATUS_ERROR;

		char * endPtr;
		// We'll always write some int value into it, so this should be fine
		int prevErrno = errno;
		errno = 0;
		long command = strtol (keyString (commandKey), &endPtr, 10);
		if (*endPtr == '\0' && errno != ERANGE)
		{
			ELEKTRA_LOG ("Child: We want to execute the command with the value %ld now", command);
			// Its hard to figure out the enum size in a portable way but for this comparison it should be ok
			switch (command)
			{
			case ELEKTRA_PLUGINPROCESS_OPEN:
				counter++;
				result = handle->kdbOpen (handle, originalKey);
				break;
			case ELEKTRA_PLUGINPROCESS_CLOSE:
				counter--;
				result = handle->kdbClose (handle, originalKey);
				break;
			case ELEKTRA_PLUGINPROCESS_GET:
				result = handle->kdbGet (handle, keySet, originalKey);
				break;
			case ELEKTRA_PLUGINPROCESS_SET:
				result = handle->kdbSet (handle, keySet, originalKey);
				break;
			case ELEKTRA_PLUGINPROCESS_ERROR:
				result = handle->kdbError (handle, keySet, originalKey);
				break;
			default:
				result = ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			ELEKTRA_LOG_DEBUG ("Child: Command executed with return value %d", result);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Child: Unrecognized command %s", keyString (commandKey));
			ELEKTRA_SET_ERRORF (191, originalKey, "Received invalid command code or no KeySet: %s", keyString (commandKey));
		}
		errno = prevErrno;
		char * resultStr = intToStr (result);
		ksAppendKey (commandKeySet, keyNew ("/pluginprocess/result", KEY_VALUE, resultStr, KEY_END));
		elektraFree (resultStr);

		ELEKTRA_LOG_DEBUG ("Child: Writing the results back to the parent");
		elektraInvoke2Args (pp->dump, "set", commandKeySet, resultPipeKey);
		if (keySet != NULL)
		{
			elektraInvoke2Args (pp->dump, "set", keySet, commandPipeKey);
			ksDel (keySet);
		}
		ELEKTRA_LOG ("Child: Command handled, startup counter is at %d", counter);
	} while (counter);

	// Final Cleanup
	ELEKTRA_LOG_DEBUG ("Child: All done, exiting the child process now");
	keyDel (commandPipeKey);
	keyDel (resultPipeKey);
	ksDel (commandKeySet);
	cleanupPluginData (pp, 0);
	// All done, exit the child process
	_Exit (EXIT_SUCCESS);
}

/** Call a plugin's function in a child process
 *
 * This will wrap all the required information to execute the given
 * command in a keyset and send it over to the child process. Then
 * it waits for the child process's answer and copies the result
 * back into the original plugin keyset and plugin key.
 *
 * Typically called like
 * @code
int elektraPluginSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_SET, returned, parentKey);

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * @param pp the data structure containing the plugin's process information
 * @param command the plugin command that should be executed, e.g. ELEKTRA_PLUGIN_GET
 * @param originalKeySet the original key set that the parent process receives
 * @param key the original key the parent process receives
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the child process communication failed
 * @retval the called plugin's return value otherwise
 * @see elektraPluginProcessIsParent for checking if we are in the parent or child process
 * @ingroup processplugin
 **/
int elektraPluginProcessSend (const ElektraPluginProcess * pp, pluginprocess_t command, KeySet * originalKeySet, Key * key)
{
	// Ensure we have a keyset when trying to call GET SET and ERROR
	if ((command == ELEKTRA_PLUGINPROCESS_GET || command == ELEKTRA_PLUGINPROCESS_SET || command == ELEKTRA_PLUGINPROCESS_ERROR) &&
	    originalKeySet == NULL)
	{
		ELEKTRA_SET_ERROR (191, key,
				   "originalKeySet has to exist when calling GET SET and ERROR via pluginprocess; but it is NULL");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// Construct the command set that controls the pluginprocess communication
	KeySet * commandKeySet = ksNew (6, KS_END);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/original", KEY_VALUE, keyName (key), KEY_END));
	ksAppendKey (commandKeySet, keyDup (key));
	char * commandStr = intToStr (command);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/command", KEY_VALUE, commandStr, KEY_END));
	elektraFree (commandStr);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/version", KEY_VALUE, "1", KEY_END));

	// Some plugin functions don't use keysets, in that case don't send any actual payload, signal via flag
	KeySet * keySet = originalKeySet != NULL ? ksDup (originalKeySet) : NULL;
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/payload/exists", KEY_VALUE, originalKeySet == NULL ? "0" : "1", KEY_END));
	ELEKTRA_LOG ("Parent: preparing everything4 %s",
		     keyString (ksLookupByName (commandKeySet, "/pluginprocess/payload/exists", KDB_O_NONE)));

	// Serialize, currently statically use dump as our default format, this already writes everything out to the pipe
	Key * commandPipeKey = keyNew ("/pluginprocess/pipe/command", KEY_VALUE, pp->commandPipe, KEY_END);
	Key * resultPipeKey = keyNew ("/pluginprocess/pipe/result", KEY_VALUE, pp->resultPipe, KEY_END);
	ELEKTRA_LOG ("Parent: Sending data to issue command %u it", command);
	elektraInvoke2Args (pp->dump, "set", commandKeySet, commandPipeKey);
	if (keySet != NULL)
	{
		ELEKTRA_LOG ("Parent: Sending the payload keyset with %zd keys through the pipe now", ksGetSize (keySet));
		// We use the two pipes "interleaved" to avoid concurrency issues that can arise when sending another keyset
		// while the pipe is currently being deserialized
		elektraInvoke2Args (pp->dump, "set", keySet, resultPipeKey);
		ELEKTRA_LOG_DEBUG ("payload sending done");
	}

	// Deserialize
	ELEKTRA_LOG_DEBUG ("Parent: Waiting for the result now");
	elektraInvoke2Args (pp->dump, "get", commandKeySet, resultPipeKey);
	if (keySet != NULL)
	{
		elektraInvoke2Args (pp->dump, "get", keySet, commandPipeKey);
		ELEKTRA_LOG ("Parent: We received %zd keys in return", ksGetSize (keySet));
	}

	// Bring everything back in order by removing our process-related keys
	Key * originalDeserializedKey = ksLookupByName (commandKeySet, keyName (key), KDB_O_NONE);
	Key * resultKey = ksLookupByName (commandKeySet, "/pluginprocess/result", KDB_O_NONE);

	// Parse the result value
	char * endPtr;
	int prevErrno = errno;
	errno = 0;
	long lresult = strtol (keyString (resultKey), &endPtr, 10);
	if (*endPtr != '\0' || errno == ERANGE || lresult > INT_MAX || lresult < INT_MIN)
	{
		ELEKTRA_SET_ERRORF (191, key, "Received invalid return code or no KeySet: %s", keyString (resultKey));
		lresult = ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		// Copy everything back into the actual keysets
		keyCopy (key, originalDeserializedKey);
		if (keySet != NULL) ksCopy (originalKeySet, keySet);
	}
	errno = prevErrno;

	// Command finished, cleanup the remaining memory now
	keyDel (commandPipeKey);
	keyDel (resultPipeKey);
	ksDel (commandKeySet);
	if (keySet != NULL) ksDel (keySet);

	return lresult; // Safe, we had a bound check before, and plugins should return values in the int range
}

/** Check if a given plugin process is the parent or the child process
 *
 * @param pp the data structure containing the plugin's process information
 * @retval 0 if it's the child process
 * @retval the child process' pid otherwise
 * @ingroup processplugin
 **/
int elektraPluginProcessIsParent (const ElektraPluginProcess * pp)
{
	return pp->pid;
}

static int elektraPluginProcessFork (ElektraPluginProcess * pp, Key * errorKey)
{
	// Prepare a named pipe so the dump plugin can serialize to it
	// Create one pipe per plugin instance / handle
	int mkfifoRet;
	if ((mkfifoRet = mkfifo (pp->commandPipe, S_IRUSR | S_IWUSR)) == -1)
	{
		ELEKTRA_SET_ERRORF (190, errorKey, "Failed to initialize the command pipe, mkfifo () returned %d", mkfifoRet);
		return -1;
	}
	if ((mkfifoRet = mkfifo (pp->resultPipe, S_IRUSR | S_IWUSR)) == -1)
	{
		ELEKTRA_SET_ERRORF (190, errorKey, "Failed to initialize the result pipe, mkfifo () returned %d", mkfifoRet);
		return -1;
	}

	pid_t pid = fork ();
	if (pid < 0)
	{
		ELEKTRA_SET_ERRORF (190, errorKey, "Failed to fork the plugin process, fork () returned %d", pid);
		return -1;
	}

	pp->pid = pid;
	ELEKTRA_LOG_DEBUG ("The pluginprocess is set using the pipes %s and %s with the pid %d", pp->commandPipe, pp->resultPipe, pid);
	return 0;
}

static char * makePipeDirectory (Key * errorKey)
{
	const char directoryNameTemplate[] = "/tmp/libelektra-pluginprocess-XXXXXX";
	char * directoryName = elektraMalloc (sizeof (directoryNameTemplate));
	strcpy (directoryName, directoryNameTemplate);

	int prevErrno = errno;
	if (mkdtemp (directoryName) == NULL || errno == ENOTDIR || errno == EINVAL)
	{
		ELEKTRA_SET_ERRORF (190, errorKey, "Failed to generate a temporary directory, mkdtemp returned errno %d", errno);
		elektraFree (directoryName);
		errno = prevErrno;
		return NULL;
	}
	errno = prevErrno;
	return directoryName;
}

static char * getPipename (const char * directoryName, const char * suffix)
{
	char * pipename = elektraMalloc (strlen (directoryName) + strlen (suffix) + 1);
	strncpy (pipename, directoryName, strlen (directoryName));
	strncpy (pipename + strlen (directoryName), suffix, strlen (suffix) + 1);
	return pipename;
}

/** Initialize a plugin to be executed in its own process
 *
 * This will prepare all the required resources and then fork the current
 * process. After the initialization the child process will typically
 * call the command loop while the parent starts to send commands to it.
 * Also the resulting process information has to be stored for the plugin.
 * In order to allow users to handle custom plugin data this will not
 * automatically call elektraPluginSetData.
 *
 * Typically called in a plugin's open function like (assuming no custom plugin data):
 * @code
int elektraPluginOpen (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		if ((pp = elektraPluginProcessInit (handle, errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		elektraPluginSetData (handle, pp);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * @param handle the plugin's handle
 * @param errorKey a key where error messages will be set
 * @retval NULL if the initialization failed
 * @retval a pointer to the information
 * @ingroup processplugin
 **/
ElektraPluginProcess * elektraPluginProcessInit (Key * errorKey)
{
	// First time initialization
	ElektraPluginProcess * pp;
	pp = elektraMalloc (sizeof (ElektraPluginProcess));
	pp->pipeDirectory = makePipeDirectory (errorKey);
	pp->commandPipe = getPipename (pp->pipeDirectory, "/command");
	pp->resultPipe = getPipename (pp->pipeDirectory, "/result");
	pp->dump = elektraInvokeOpen ("dump", 0, errorKey);
	pp->counter = 0;
	pp->pluginData = NULL;

	if (pp->pipeDirectory && pp->commandPipe && pp->resultPipe && pp->dump)
	{
		if (elektraPluginProcessFork (pp, errorKey) < 0)
		{
			cleanupPluginData (pp, errorKey);
			return NULL;
		}
	}
	else
	{
		cleanupPluginData (pp, errorKey);
		ELEKTRA_SET_ERROR (190, errorKey, "Failed to initialize the pipenames and the dump plugin");
		return NULL;
	}
	return pp;
}

/** Call a plugin's open function in a child process
 *
 * This will increase the internal counter how often open/close has been called,
 * so the opened pipes and forks will not be closed too early.
 *
 * @param pp the data structure containing the plugin's process information
 * @param errorKey a key where error messages will be set
 * @retval the return value of the plugin's open function
 * @see elektraPluginProcessInit for an example how and where this function is typically used
 * @ingroup processplugin
 **/
int elektraPluginProcessOpen (ElektraPluginProcess * pp, Key * errorKey)
{
	pp->counter = pp->counter + 1;
	return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_OPEN, NULL, errorKey);
}

/** Cleanup a plugin process
 *
 * This will decrease the internal counter how often open/close has been called,
 * closing opened pipes when the counter reaches 0. This will not delete the
 * plugin data associated with the handle as it may contain other data out of
 * the scope of this library, so this has to be done manually like
 * @code
int elektraPluginClose (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) {
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp) elektraPluginSetData (handle, NULL);
		return result.result;
	}

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * @param pp the data structure containing the plugin's process information
 * @retval 1 if the data structure got cleaned up
 * @retval 0 if the data structure is still used
 * @ingroup processplugin
 **/
ElektraPluginProcessCloseResult elektraPluginProcessClose (ElektraPluginProcess * pp, Key * errorKey)
{
	pp->counter = pp->counter - 1;
	int result = elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_CLOSE, NULL, errorKey);
	int done = pp->counter <= 0;
	if (done) cleanupPluginData (pp, errorKey);
	ElektraPluginProcessCloseResult closeResult = { result, done };
	return closeResult;
}

/** Store a pointer to any plugin related data that is being executed inside an own process.
 *
 * @param plugin a pointer to the plugin
 * @param data the pointer to the data
 */
void elektraPluginProcessSetData (Plugin * handle, void * data)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp) pp->pluginData = data;
}

/** Get a pointer to any plugin related data stored before.
 *
 * If elektraPluginProcessSetData was not called earlier, NULL will be returned.
 *
 * @param plugin a pointer to the plugin
 * @retval a pointer to the data
 */
void * elektraPluginProcessGetData (Plugin * handle)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp) return pp->pluginData;
	return NULL;
}
