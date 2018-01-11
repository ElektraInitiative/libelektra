/**
 * @file
 *
 * @brief Source for the pluginprocess library
 *
 * Executes plugins in a separate process via fork and uses a simple
 * communication protocol based on the dump plugin via named pipes.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbpluginprocess.h"
#include <kdbinvoke.h>
// To access the plugin function pointers
#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct _ElektraPluginProcess
{
	char * commandPipe;
	char * resultPipe;
	int pid;
	int counter;
	ElektraInvokeHandle * dump;
};

static void cleanupPluginData (ElektraPluginProcess * pp)
{
	elektraInvokeClose (pp->dump);
	if (pp->pid) unlink (pp->commandPipe);
	elektraFree (pp->commandPipe);
	if (pp->pid) unlink (pp->resultPipe);
	elektraFree (pp->resultPipe);
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
	Key * commandPipeKey = keyNew ("/pluginprocess/commandPipe", KEY_VALUE, pp->commandPipe, KEY_END);
	Key * resultPipeKey = keyNew ("/pluginprocess/resultPipe", KEY_VALUE, pp->resultPipe, KEY_END);
	KeySet * keySet = ksNew (0, KS_END);
	int counter = 0;
	plugin_t command = ELEKTRA_PLUGIN_END;

	do
	{
		ELEKTRA_LOG_DEBUG ("C: Wait for a KeySet");
		elektraInvoke2Args (pp->dump, "get", keySet, commandPipeKey);
		ELEKTRA_LOG_DEBUG ("C: We received a KeySet with %zd keys in it", ksGetSize (keySet));
		Key * commandKey = ksLookupByName (keySet, "/pluginprocess/command", KDB_O_POP);
		char * endPtr;
		// We'll always write some int value into it, so this should be fine
		int prevErrno = errno;
		errno = 0;
		command = (int)strtol (keyString (commandKey), &endPtr, 10);

		Key * originalNameKey = ksLookupByName (keySet, "/pluginprocess/original", KDB_O_POP);
		Key * originalKey = ksLookupByName (keySet, keyString (originalNameKey), KDB_O_NONE);

		int result = ELEKTRA_PLUGIN_STATUS_ERROR;
		if (*endPtr == '\0' && errno != ERANGE)
		{
			ELEKTRA_LOG ("C: We want to execute the command with the value %d now", command);
			switch (command)
			{
			case ELEKTRA_PLUGIN_OPEN:
				counter++;
				result = handle->kdbOpen (handle, originalKey);
				break;
			case ELEKTRA_PLUGIN_CLOSE:
				counter--;
				result = handle->kdbClose (handle, originalKey);
				break;
			case ELEKTRA_PLUGIN_GET:
				result = handle->kdbGet (handle, keySet, originalKey);
				break;
			case ELEKTRA_PLUGIN_SET:
				result = handle->kdbSet (handle, keySet, originalKey);
				break;
			case ELEKTRA_PLUGIN_ERROR:
				result = handle->kdbError (handle, keySet, originalKey);
				break;
			default:
				result = ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			ELEKTRA_LOG_DEBUG ("C: Command executed with return value %d", result);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("C: Unrecognized command %s", keyString (commandKey));
			ELEKTRA_ADD_WARNINGF (191, originalKey, "Received invalid command code or no KeySet: %s", keyString (commandKey));
		}
		errno = prevErrno;
		char * resultStr = intToStr (result);
		ksAppendKey (keySet, keyNew ("/pluginprocess/result", KEY_VALUE, resultStr, KEY_END));
		elektraFree (resultStr);

		ELEKTRA_LOG_DEBUG ("C: Writing the resulting KeySet back to the parent");
		elektraInvoke2Args (pp->dump, "set", keySet, resultPipeKey);
		keyDel (commandKey);
		keyDel (originalNameKey);
		ELEKTRA_LOG ("C: Command handled, startup counter is at %d", counter);
	} while (counter);

	// Final Cleanup
	ELEKTRA_LOG_DEBUG ("C: All done, exiting the child process now");
	keyDel (commandPipeKey);
	keyDel (resultPipeKey);
	ksDel (keySet);
	cleanupPluginData (pp);
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
int elektraPluginProcessSend (const ElektraPluginProcess * pp, plugin_t command, KeySet * originalKeySet, Key * key)
{
	// Some plugin functions don't use keysets, but we need one for our
	// simple communication protocol, so create a temporary one
	KeySet * keySet = originalKeySet == NULL ? ksNew (3, KS_END) : ksDup (originalKeySet);
	ksAppendKey (keySet, keyDup (key));
	char * commandStr = intToStr (command);
	ksAppendKey (keySet, keyNew ("/pluginprocess/command", KEY_VALUE, commandStr, KEY_END));
	Key * originalKey = keyNew ("/pluginprocess/original", KEY_VALUE, keyName (key), KEY_END);
	ksAppendKey (keySet, originalKey);

	// Serialize, currently statically use dini our default format, this already writes everything out to the pipe
	Key * commandPipeKey = keyNew ("/pluginprocess/commandPipe", KEY_VALUE, pp->commandPipe, KEY_END);
	Key * resultPipeKey = keyNew ("/pluginprocess/resultPipe", KEY_VALUE, pp->resultPipe, KEY_END);
	ELEKTRA_LOG ("P: We send %zd keys through the pipe now to issue command %d it", ksGetSize (keySet), command);
	elektraInvoke2Args (pp->dump, "set", keySet, commandPipeKey);
	ELEKTRA_LOG_DEBUG ("P: Waiting for the result now");
	elektraInvoke2Args (pp->dump, "get", keySet, resultPipeKey);
	ELEKTRA_LOG ("P: We received %zd keys in return", ksGetSize (keySet));

	// Bring everything back in order by removing our process-related keys
	Key * originalDeserializedKey = ksLookupByName (keySet, keyName (key), KDB_O_POP);
	Key * resultKey = ksLookupByName (keySet, "/pluginprocess/result", KDB_O_POP);
	char * endPtr;
	int prevErrno = errno;
	errno = 0;
	int result = (int)strtol (keyString (resultKey), &endPtr, 10);
	// Copy everything back into the actual keysets
	keyCopy (key, originalDeserializedKey);
	ksCopy (originalKeySet, keySet);

	// Command finished, cleanup now
	keyDel (commandPipeKey);
	keyDel (resultPipeKey);
	keyDel (originalDeserializedKey);
	ksDel (keySet);

	if (*endPtr != '\0' || errno == ERANGE)
	{
		ELEKTRA_ADD_WARNINGF (191, originalKey, "Received invalid return code or no KeySet: %s", keyString (resultKey));
		keyDel (resultKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	keyDel (resultKey);
	errno = prevErrno;

	return result;
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

static int elektraPluginProcessFork (Plugin * handle, Key * errorKey, ElektraPluginProcess * pp)
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
	ELEKTRA_LOG_DEBUG ("The pluginprocess handle is %p with the pid %d", (void *)handle, pid);
	return 0;
}

static char * getPipename (Plugin * handle, char * suffix)
{
	int pipenameSize;
	char * pipename;
	pipenameSize = snprintf (NULL, 0, "/tmp/libelektra-process-%p-%s", (void *)handle, suffix);
	pipename = elektraMalloc (pipenameSize + 1);
	pipenameSize = snprintf (pipename, pipenameSize + 1, "/tmp/libelektra-process-%p-%s", (void *)handle, suffix);
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
ElektraPluginProcess * elektraPluginProcessInit (Plugin * handle, Key * errorKey)
{
	// First time initialization
	ElektraPluginProcess * pp;
	pp = elektraMalloc (sizeof (ElektraPluginProcess));
	pp->commandPipe = getPipename (handle, "command");
	pp->resultPipe = getPipename (handle, "result");
	pp->dump = elektraInvokeOpen ("dump", 0);
	pp->counter = 0;
	if (pp->commandPipe && pp->resultPipe && pp->dump)
	{
		if (elektraPluginProcessFork (handle, errorKey, pp) < 0)
		{
			cleanupPluginData (pp);
			return NULL;
		}
	}
	else
	{
		cleanupPluginData (pp);
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
	return elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_OPEN, NULL, errorKey);
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
		int result = elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_CLOSE, NULL, errorKey);
		if (elektraPluginProcessClose (pp)) elektraPluginSetData (handle, NULL);
		return result;
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
int elektraPluginProcessClose (ElektraPluginProcess * pp)
{
	pp->counter = pp->counter - 1;
	if (!pp->counter) cleanupPluginData (pp);
	return pp->counter <= 0;
}
