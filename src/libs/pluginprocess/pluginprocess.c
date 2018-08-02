/**
 * @file
 *
 * @brief Source for the pluginprocess library
 *
 * Executes plugins in a separate process via fork and uses a simple
 * communication protocol based on the dump plugin via named pipes.
 *
 * The communication protocol works as follows, where Child and Parent stand
 * for the child and the parent process:
 * 1)  Two named pipes are created, called commandPipe and resultPipe
       - commandPipe is used for a keyset containing metainformation about
       the communicationprotocol
       - resultPipe is used for transferring the actual payload that gets passed
       to a plugin
 * 2)  The parent constructs a keyset called commandKeySet, containing:
 *     - /pluginprocess/parent
 *       a copy of the parent key passed to the plugin interface
 *     - /pluginprocess/parent/name
 *       the name of the parent key passed to the plugin interface
 *     - /pluginprocess/command
 *       the command that should be executed by the child process
 *     - /pluginprocess/payload/size
 *       the size of the payload keyset, -1 if there is no payload
 *     - /pluginprocess/version
 *       a number indicating the version of this communication
 *       protocol
 * 3)  The parent sends over the commandKeySet over the commandPipe
 * 4)  For operations requiring a keyset, the parent sends the
 *     keyset (originalKeySet) that is passed to this plugin over the resultPipe
 * 5)  Child receives the commandKeySet
 * 6)  Child receives the keyset if one exists
 * 7)  Child executes the plugin on the keyset with the originalKey
 * 8)  Child adds the result value of the plugin operation to the commandKeySet
       into the key /pluginprocess/result
 * 9)  Child sends the commandKeySet over the commandPipe
 * 10) For operations requiring a keyset, Child sends the keyset
 *    over the resultPipe
 * 11) Parent receives the commandKeySet and interpretes /pluginprocess/result
 * 12) For operations requiring a keyset, Parent receives the keyset
 *     and copies it back to originalKeySet set
 * 13) Parent returns the result value from the child process
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
	int counter = 0;

	do
	{
		KeySet * commandKeySet = ksNew (6, KS_END);
		KeySet * keySet = NULL;
		ELEKTRA_LOG_DEBUG ("Child: Wait for commands");
		elektraInvoke2Args (pp->dump, "get", commandKeySet, commandPipeKey);

		Key * payloadSizeKey = ksLookupByName (commandKeySet, "/pluginprocess/payload/size", KDB_O_NONE);
		char * endPtr;
		// We'll always write some int value into it, so this should be fine
		int prevErrno = errno;
		errno = 0;
		long payloadSize = strtol (keyString (payloadSizeKey), &endPtr, 10);
		// in case the payload size fails to be transferred, that it shouldn't, we can only assume no payload
		if (*endPtr == '\0' && errno != ERANGE && payloadSize >= 0)
		{
			keySet = ksNew (payloadSize, KS_END);
			elektraInvoke2Args (pp->dump, "get", keySet, resultPipeKey);
			ELEKTRA_LOG_DEBUG ("Child: We received a KeySet with %zd keys in it", ksGetSize (keySet));
		}
		errno = prevErrno;

		Key * commandKey = ksLookupByName (commandKeySet, "/pluginprocess/command", KDB_O_NONE);
		Key * parentNameKey = ksLookupByName (commandKeySet, "/pluginprocess/parent/name", KDB_O_NONE);
		Key * parentKey = ksLookupByName (commandKeySet, "/pluginprocess/parent", KDB_O_POP);
		Key * key = keyDup (parentKey);
		keySetName (key, keyString (parentNameKey));
		int result = ELEKTRA_PLUGIN_STATUS_ERROR;

		// We'll always write some int value into it, so this should be fine
		prevErrno = errno;
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
				result = handle->kdbOpen (handle, key);
				break;
			case ELEKTRA_PLUGINPROCESS_CLOSE:
				counter--;
				result = handle->kdbClose (handle, key);
				break;
			case ELEKTRA_PLUGINPROCESS_GET:
				result = handle->kdbGet (handle, keySet, key);
				break;
			case ELEKTRA_PLUGINPROCESS_SET:
				result = handle->kdbSet (handle, keySet, key);
				break;
			case ELEKTRA_PLUGINPROCESS_ERROR:
				result = handle->kdbError (handle, keySet, key);
				break;
			default:
				result = ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			ELEKTRA_LOG_DEBUG ("Child: Command executed with return value %d", result);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Child: Unrecognized command %s", keyString (commandKey));
			ELEKTRA_SET_ERRORF (191, key, "Received invalid command code or no KeySet: %s", keyString (commandKey));
		}
		errno = prevErrno;
		char * resultStr = intToStr (result);
		ksAppendKey (commandKeySet, keyNew ("/pluginprocess/result", KEY_VALUE, resultStr, KEY_END));
		elektraFree (resultStr);
		keySetName (key, "/pluginprocess/parent");
		ksAppendKey (commandKeySet, key);
		keyDel (parentKey);

		ELEKTRA_LOG_DEBUG ("Child: Writing the results back to the parent");
		elektraInvoke2Args (pp->dump, "set", commandKeySet, commandPipeKey);
		if (keySet != NULL)
		{
			elektraInvoke2Args (pp->dump, "set", keySet, resultPipeKey);
			ksDel (keySet);
		}
		ksDel (commandKeySet);
		ELEKTRA_LOG ("Child: Command handled, startup counter is at %d", counter);
	} while (counter);

	// Final Cleanup
	ELEKTRA_LOG_DEBUG ("Child: All done, exiting the child process now");
	keyDel (commandPipeKey);
	keyDel (resultPipeKey);
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
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * @param pp the data structure containing the plugin's process information
 * @param command the plugin command that should be executed, e.g. ELEKTRA_PLUGINPROCESS_GET
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
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/parent/name", KEY_VALUE, keyName (key), KEY_END));
	Key * parentKey = keyDup (key);
	keySetName (parentKey, "/pluginprocess/parent");
	ksAppendKey (commandKeySet, parentKey);
	char * commandStr = intToStr (command);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/command", KEY_VALUE, commandStr, KEY_END));
	elektraFree (commandStr);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/version", KEY_VALUE, "1", KEY_END));

	// Some plugin functions don't use keysets, in that case don't send any actual payload, signal via flag
	KeySet * keySet = originalKeySet != NULL ? ksDup (originalKeySet) : NULL;
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/payload/size", KEY_VALUE,
					    originalKeySet == NULL ? "-1" : intToStr (ksGetSize (originalKeySet)), KEY_END));

	// Serialize, currently statically use dump as our default format, this already writes everything out to the pipe
	Key * commandPipeKey = keyNew ("/pluginprocess/pipe/command", KEY_VALUE, pp->commandPipe, KEY_END);
	Key * resultPipeKey = keyNew ("/pluginprocess/pipe/result", KEY_VALUE, pp->resultPipe, KEY_END);
	ELEKTRA_LOG ("Parent: Sending data to issue command %u it", command);
	elektraInvoke2Args (pp->dump, "set", commandKeySet, commandPipeKey);
	if (keySet != NULL)
	{
		ELEKTRA_LOG ("Parent: Sending the payload keyset with %zd keys through the pipe now", ksGetSize (keySet));
		elektraInvoke2Args (pp->dump, "set", keySet, resultPipeKey);
	}

	// Deserialize
	ELEKTRA_LOG_DEBUG ("Parent: Waiting for the result now");
	elektraInvoke2Args (pp->dump, "get", commandKeySet, commandPipeKey);
	if (keySet != NULL)
	{
		elektraInvoke2Args (pp->dump, "get", keySet, resultPipeKey);
		ELEKTRA_LOG ("Parent: We received %zd keys in return", ksGetSize (keySet));
	}

	// Bring everything back in order by removing our process-related keys
	Key * parentDeserializedKey = ksLookupByName (commandKeySet, "/pluginprocess/parent", KDB_O_NONE);
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
		int parentKeyExistsInOriginalKeySet = keySet != NULL ? ksLookup (originalKeySet, key, KDB_O_POP) != NULL : 0;
		// Copy everything back into the actual keysets
		// Unfortunately we can't use keyCopy here as ksAppendKey locks it so it will fail
		// This is the case if the parent key is also contained in the originalKeySet / has been appended
		// As an invariant we assume plugins don't change the parent key's name during a plugin call
		// This would interfere with keyset memberships
		keySetString (key, keyString (parentDeserializedKey));
		// Clear metadata before, we allow children to modify it
		keyRewindMeta (key);
		const Key * currentMeta;
		while ((currentMeta = keyNextMeta (key)) != NULL)
		{
			keySetMeta (key, keyName (currentMeta), 0);
		}
		keyCopyAllMeta (key, parentDeserializedKey);
		if (keySet != NULL)
		{
			// in case originalKeySet contains key this would make it stuck
			// thus remove it here and re-add it afterwards
			ksCopy (originalKeySet, keySet);
			if (parentKeyExistsInOriginalKeySet) ksAppendKey (originalKeySet, key);
		}
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
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
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
	int result = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	if (pp->counter > 0)
	{
		pp->counter = pp->counter - 1;
		result = elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_CLOSE, NULL, errorKey);
	}
	int done = pp->counter <= 0;
	if (done) cleanupPluginData (pp, errorKey);
	ElektraPluginProcessCloseResult closeResult = { result, done };
	return closeResult;
}

/** Store a pointer to any plugin related data that is being executed inside an own process.
 *
 * This is required in case additional arbitrary plugin data should be stored. Pluginprocess
 * has to be stored using elektraPluginSetData. Plugin data for the child process
 * has to be stored using this function like
 * @code
int elektraPluginOpen (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		ArbitraryPluginData * data = // initialize your plugin data
		elektraPluginProcessSetData (handle, data);
		elektraPluginSetData (handle, pp);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * Furthermore ensure to cleanup the data after the plugin is done like
 * @code
int elektraPluginClose (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) {
		ArbitraryPluginData * data = elektraPluginProcessGetData (handle);
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp)
		{
			elektraPluginSetData (handle, NULL);
			// cleanup data here, this was the last call to the plugin
		}
		return result.result;
	}

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * This way you can use elektraPluginProcessGetData (handle) in your child process
 * to get the data you want your plugin to work with.
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
