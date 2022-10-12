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
 * 1)  Four pipes are created to handle the communication in a reliable way
       - parentCommandPipe is used for a keyset containing metainformation about
       the communication protocol, going from Parent to Child
       - parentPayloadPipe is used for transferring the actual payload that gets passed
       to a plugin, going from Parent to Child
       - childCommandPipe is used for a keyset containing metainformation about
       the communication protocol, going from Child to Parent
       - childPayloadPipe is used for transferring the actual payload that gets passed
       to a plugin, going from Child to Parent
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
 * 3)  The parent sends over the commandKeySet over the parentCommandPipe
 * 4)  For operations requiring a keyset, the parent sends the
 *     keyset (originalKeySet) that is passed to this plugin over the parentPayloadPipe
 * 5)  Child receives the commandKeySet
 * 6)  Child receives the keyset if one exists
 * 7)  Child executes the plugin on the keyset with the originalKey
 * 8)  Child adds the result value of the plugin operation to the commandKeySet
       into the key /pluginprocess/result
 * 9)  Child sends the commandKeySet over the childCommandPipe
 * 10) For operations requiring a keyset, Child sends the keyset
 *    over the childPayloadPipe
 * 11) Parent receives the commandKeySet and interprets /pluginprocess/result
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
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct _ElektraPluginProcess
{
	int parentCommandPipe[2];
	int parentPayloadPipe[2];
	int childCommandPipe[2];
	int childPayloadPipe[2];

	Key * parentCommandPipeKey;
	Key * parentPayloadPipeKey;
	Key * childCommandPipeKey;
	Key * childPayloadPipeKey;

	int pid;
	int counter;
	ElektraInvokeHandle * dump;
	void * pluginData;
};

static void cleanupPluginData (ElektraPluginProcess * pp, Key * errorKey, int cleanAllPipes)
{
	if (pp->dump) elektraInvokeClose (pp->dump, errorKey);

	if (pp->parentCommandPipeKey) keyDel (pp->parentCommandPipeKey);
	if (pp->parentPayloadPipeKey) keyDel (pp->parentPayloadPipeKey);
	if (pp->childCommandPipeKey) keyDel (pp->childCommandPipeKey);
	if (pp->childPayloadPipeKey) keyDel (pp->childPayloadPipeKey);

	// twisted way to clean either both ends if something failed upon or before forking
	// and the used ends otherwise
	for (int pipeIdx = !elektraPluginProcessIsParent (pp); pipeIdx <= cleanAllPipes; ++pipeIdx)
	{
		if (pp->parentCommandPipe[!pipeIdx]) close (pp->parentCommandPipe[!pipeIdx]);
		if (pp->parentPayloadPipe[!pipeIdx]) close (pp->parentPayloadPipe[!pipeIdx]);
		if (pp->childCommandPipe[pipeIdx]) close (pp->childCommandPipe[pipeIdx]);
		if (pp->childPayloadPipe[pipeIdx]) close (pp->childPayloadPipe[pipeIdx]);
	}

	elektraFree (pp);
}

static char * longToStr (long i)
{
	long size;
	char * str;
	size = snprintf (NULL, 0, "%ld", i);
	str = elektraMalloc (size + 1);
	size = snprintf (str, size + 1, "%ld", i);
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
	int counter = 0;

	do
	{
		KeySet * commandKeySet = ksNew (6, KS_END);
		KeySet * keySet = NULL;
		ELEKTRA_LOG_DEBUG ("Child: Wait for commands on pipe %s", keyString (pp->parentCommandPipeKey));
		elektraInvoke2Args (pp->dump, "get", commandKeySet, pp->parentCommandPipeKey);

		if (ksGetSize (commandKeySet) == 0)
		{
			ELEKTRA_LOG_DEBUG ("Child: Failed to read from parentCommandPipe, exiting");
			ksDel (commandKeySet);
			break;
		}

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
			elektraInvoke2Args (pp->dump, "get", keySet, pp->parentPayloadPipeKey);
			ELEKTRA_LOG_DEBUG ("Child: We received a KeySet with %zd keys in it", ksGetSize (keySet));
		}
		errno = prevErrno;

		Key * commandKey = ksLookupByName (commandKeySet, "/pluginprocess/command", KDB_O_NONE);
		Key * parentNameKey = ksLookupByName (commandKeySet, "/pluginprocess/parent/name", KDB_O_NONE);
		Key * parentKey = ksLookupByName (commandKeySet, "/pluginprocess/parent", KDB_O_POP);
		Key * key = keyDup (parentKey, KEY_CP_ALL);
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
			case ELEKTRA_PLUGINPROCESS_COMMIT:
				result = handle->kdbCommit (handle, keySet, key);
				break;
			case ELEKTRA_PLUGINPROCESS_INIT:
				result = handle->kdbInit (handle, keySet, key);
				break;
			default:
				result = ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			ELEKTRA_LOG_DEBUG ("Child: Command executed with return value %d", result);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Child: Unrecognized command %s", keyString (commandKey));
			ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (key, "Received invalid command code or no KeySet from child process: %s",
							       keyString (commandKey));
		}
		errno = prevErrno;
		char * resultStr = longToStr (result);
		ksAppendKey (commandKeySet, keyNew ("/pluginprocess/result", KEY_VALUE, resultStr, KEY_END));
		elektraFree (resultStr);
		keySetName (key, "/pluginprocess/parent");
		ksAppendKey (commandKeySet, key);
		keyDel (parentKey);

		ELEKTRA_LOG_DEBUG ("Child: Writing the results back to the parent");
		elektraInvoke2Args (pp->dump, "set", commandKeySet, pp->childCommandPipeKey);
		if (keySet != NULL)
		{
			char * resultPayloadSize = longToStr (ksGetSize (keySet));
			keySetString (payloadSizeKey, resultPayloadSize);
			elektraFree (resultPayloadSize);
			elektraInvoke2Args (pp->dump, "set", keySet, pp->childPayloadPipeKey);
			ksDel (keySet);
		}
		ksDel (commandKeySet);
		ELEKTRA_LOG ("Child: Command handled, startup counter is at %d", counter);
	} while (counter);

	// Final Cleanup
	ELEKTRA_LOG_DEBUG ("Child: All done, exiting the child process now");
	cleanupPluginData (pp, 0, 1);
	// All done, exit the child process so it won't do any actual effects in elektra
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
		ELEKTRA_SET_INTERFACE_ERROR (
			key, "Variable originalKeySet has to exist when calling GET SET and ERROR via pluginprocess but it is NULL");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// Construct the command set that controls the pluginprocess communication
	KeySet * commandKeySet = ksNew (6, KS_END);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/parent/name", KEY_VALUE, keyName (key), KEY_END));
	Key * parentKey = keyDup (key, KEY_CP_ALL);
	keySetName (parentKey, "/pluginprocess/parent");
	ksAppendKey (commandKeySet, parentKey);
	char * commandStr = longToStr (command);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/command", KEY_VALUE, commandStr, KEY_END));
	elektraFree (commandStr);
	ksAppendKey (commandKeySet, keyNew ("/pluginprocess/version", KEY_VALUE, "1", KEY_END));

	// Some plugin functions don't use keysets, in that case don't send any actual payload, signal via flag
	KeySet * keySet = originalKeySet != NULL ? ksDup (originalKeySet) : NULL;
	char * payloadSizeStr = longToStr (ksGetSize (originalKeySet));
	ksAppendKey (commandKeySet,
		     keyNew ("/pluginprocess/payload/size", KEY_VALUE, originalKeySet == NULL ? "-1" : payloadSizeStr, KEY_END));
	elektraFree (payloadSizeStr);

	// Serialize, currently statically use dump as our default format, this already writes everything out to the pipe
	ELEKTRA_LOG ("Parent: Sending data to issue command %u it through pipe %s", command, keyString (pp->parentCommandPipeKey));
	elektraInvoke2Args (pp->dump, "set", commandKeySet, pp->parentCommandPipeKey);
	if (keySet != NULL)
	{
		ELEKTRA_LOG ("Parent: Sending the payload keyset with %zd keys through the pipe %s", ksGetSize (keySet),
			     keyString (pp->parentPayloadPipeKey));
		elektraInvoke2Args (pp->dump, "set", keySet, pp->parentPayloadPipeKey);
	}

	// Deserialize
	ELEKTRA_LOG_DEBUG ("Parent: Waiting for the result now on pipe %s", keyString (pp->childCommandPipeKey));
	elektraInvoke2Args (pp->dump, "get", commandKeySet, pp->childCommandPipeKey);

	if (keySet != NULL)
	{
		// clear the keyset before to avoid memleaks caused by dump
		char * endPtr;
		int prevErrno = errno;
		errno = 0;
		long payloadSize =
			strtol (keyString (ksLookupByName (commandKeySet, "/pluginprocess/payload/size", KDB_O_NONE)), &endPtr, 10);
		// in case the payload size fails to be transferred, that it shouldn't, we simply assume the previous size
		if (*endPtr != '\0' || errno == ERANGE || payloadSize < 0) payloadSize = ksGetSize (keySet);
		errno = prevErrno;
		ksDel (keySet);
		keySet = ksNew (payloadSize, KS_END);
		elektraInvoke2Args (pp->dump, "get", keySet, pp->childPayloadPipeKey);
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
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (key, "Received invalid return code or no KeySet from child process: %s",
						       keyString (resultKey));
		lresult = ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else // Copy everything back into the actual keysets
	{
		Key * parentKeyInOriginalKeySet = keySet != NULL ? ksLookup (originalKeySet, key, KDB_O_NONE) : NULL;
		// maybe there are just 2 keys with the same name, can happen in theory, so compare memory
		int parentKeyExistsInOriginalKeySet = parentKeyInOriginalKeySet == key;
		// if the child added the parent key to the keyset pop it from the keyset
		// then reinsert key after we copied the data and delete this serialized copy
		Key * parentKeyInKeySet = keySet != NULL ? ksLookup (keySet, key, KDB_O_POP) : NULL;
		int childAddedParentKey = parentKeyInKeySet != NULL;

		// Unfortunately we can't use keyCopy here as ksAppendKey locks it so it will fail
		// This is the case if the parent key is also contained in the originalKeySet / has been appended
		// As an invariant we assume plugins don't change the parent key's name during a plugin call
		// This would interfere with keyset memberships
		keySetString (key, keyString (parentDeserializedKey));

		KeySet * metaKeys = keyMeta (key);
		for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
		{
			const Key * currentMeta = ksAtCursor (metaKeys, it);
			keySetMeta (key, keyName (currentMeta), 0);
		}

		keyCopyAllMeta (key, parentDeserializedKey);
		if (childAddedParentKey) keyCopyAllMeta (key, parentKeyInKeySet);

		if (keySet != NULL)
		{
			// in case originalKeySet contains key this would make it stuck
			// thus remove it here and re-add it afterwards
			if (parentKeyExistsInOriginalKeySet) ksLookup (originalKeySet, parentKeyInOriginalKeySet, KDB_O_POP);
			ksCopy (originalKeySet, keySet);
			if (parentKeyExistsInOriginalKeySet || childAddedParentKey) ksAppendKey (originalKeySet, key);
			if (childAddedParentKey) keyDel (parentKeyInKeySet);
		}
	}
	errno = prevErrno;

	// Command finished, cleanup the remaining memory now
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
	return pp->pid != 0;
}

static char * concat (const char * str1, const char * str2)
{
	const int str1Len = strlen (str1);
	const int str2Len = strlen (str2);
	char * concat = elektraMalloc (str1Len + str2Len + 1);
	strcpy (concat, str1);
	strncpy (concat + str1Len, str2, str2Len + 1);
	return concat;
}

static int makePipe (ElektraPluginProcess * pp, Key * errorKey, const char * pipeName, int pipeRef[2])
{
	int ret;
	if ((ret = pipe (pipeRef)))
	{
		cleanupPluginData (pp, errorKey, 1);
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "Failed to initialize %s, pipe () returned %d", pipeName, ret);
		return 0;
	}
	return 1;
}

static Key * makePipeKey (const char * pipeName, const int pipeFd)
{
	// create the key for this pipe for the use with dump
	Key * key;

	char * pipeFdStr = longToStr (pipeFd);
	char * pipeKeyValue = concat ("/dev/fd/", pipeFdStr);
	elektraFree (pipeFdStr);
	char * pipeKeyName = concat ("/pluginprocess/pipe/", pipeName);
	key = keyNew (pipeKeyName, KEY_VALUE, pipeKeyValue, KEY_END);
	elektraFree (pipeKeyName);
	elektraFree (pipeKeyValue);

	return key;
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
	pp->counter = 0;
	pp->pluginData = NULL;
	pp->parentCommandPipeKey = NULL;
	pp->parentPayloadPipeKey = NULL;
	pp->childCommandPipeKey = NULL;
	pp->childPayloadPipeKey = NULL;

	KeySet * config = ksNew (1, keyNew ("user:/fullname", KEY_END), KS_END);
	pp->dump = elektraInvokeOpen ("dump", config, errorKey);
	ksDel (config);

	if (!pp->dump)
	{
		cleanupPluginData (pp, errorKey, 0);
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Failed to initialize the dump plugin");
		return NULL;
	}

	// As generally recommended, ignore SIGPIPE because we will notice that the
	// commandKeySet has been transferred incorrectly anyway to detect broken pipes
	signal (SIGPIPE, SIG_IGN);

	// Prepare the pipes
	if (!makePipe (pp, errorKey, "parentCommandPipe", pp->parentCommandPipe) ||
	    !makePipe (pp, errorKey, "parentPayloadPipe", pp->parentPayloadPipe) ||
	    !makePipe (pp, errorKey, "childCommandPipe", pp->childCommandPipe) ||
	    !makePipe (pp, errorKey, "childPayloadPipe", pp->childPayloadPipe))
		return NULL;

	pp->pid = fork ();

	if (pp->pid < 0)
	{
		cleanupPluginData (pp, errorKey, 1);
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "Failed to fork the plugin process, fork () returned %d", pp->pid);
		return NULL;
	}

	int pipeIdx = elektraPluginProcessIsParent (pp);
	close (pp->parentCommandPipe[!pipeIdx]);
	close (pp->parentPayloadPipe[!pipeIdx]);
	close (pp->childCommandPipe[pipeIdx]);
	close (pp->childPayloadPipe[pipeIdx]);

	ELEKTRA_LOG_DEBUG ("parentCommandPipe[%d] has file descriptor %d", pipeIdx, pp->parentCommandPipe[pipeIdx]);
	ELEKTRA_LOG_DEBUG ("parentPayloadPipe[%d] has file descriptor %d", pipeIdx, pp->parentPayloadPipe[pipeIdx]);
	ELEKTRA_LOG_DEBUG ("childCommandPipe[%d] has file descriptor %d", !pipeIdx, pp->childCommandPipe[!pipeIdx]);
	ELEKTRA_LOG_DEBUG ("childPayloadPipe[%d] has file descriptor %d", !pipeIdx, pp->childPayloadPipe[!pipeIdx]);

	// Prepare the keys for the pipes to use with dump
	pp->parentCommandPipeKey = makePipeKey ("parentCommandPipe", pp->parentCommandPipe[pipeIdx]);
	pp->parentPayloadPipeKey = makePipeKey ("parentPayloadPipe", pp->parentPayloadPipe[pipeIdx]);
	pp->childCommandPipeKey = makePipeKey ("childCommandPipe", pp->childCommandPipe[!pipeIdx]);
	pp->childPayloadPipeKey = makePipeKey ("childPayloadPipe", pp->childPayloadPipe[!pipeIdx]);

	ELEKTRA_LOG_DEBUG ("parentCommandPipeKey is %s on %d", keyString (pp->parentCommandPipeKey), pp->pid);
	ELEKTRA_LOG_DEBUG ("parentPayloadPipeKey is %s on %d", keyString (pp->parentPayloadPipeKey), pp->pid);
	ELEKTRA_LOG_DEBUG ("childCommandPipeKey is %s on %d", keyString (pp->childCommandPipeKey), pp->pid);
	ELEKTRA_LOG_DEBUG ("childPayloadPipeKey is %s on %d", keyString (pp->childPayloadPipeKey), pp->pid);

	ELEKTRA_LOG_DEBUG ("The pluginprocess is set with the pid %d", pp->pid);
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
	if (pp && elektraPluginProcessIsParent (pp)) {
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp) elektraPluginSetData (handle, NULL);
		return result.result;
	}

	// actual plugin functionality to be executed in a child process
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
 * @endcode
 *
 * Note that pp might be null here if the initialization failed!
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
	if (done) cleanupPluginData (pp, errorKey, 0);
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
		elektraPluginProcessSetData (pp, data);
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
		ArbitraryPluginData * data = elektraPluginProcessGetData (pp);
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
 * @param pp the data structure containing the plugin's process information
 * @param data the pointer to the data
 */
void elektraPluginProcessSetData (ElektraPluginProcess * pp, void * data)
{
	if (pp) pp->pluginData = data;
}

/** Get a pointer to any plugin related data stored before.
 *
 * If elektraPluginProcessSetData was not called earlier, NULL will be returned.
 *
 * @param pp the data structure containing the plugin's process information
 * @retval a pointer to the data
 */
void * elektraPluginProcessGetData (const ElektraPluginProcess * pp)
{
	if (pp) return pp->pluginData;
	return NULL;
}
