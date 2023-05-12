/**
 * @file
 *
 * @brief Source for recorder plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "recorder.h"

#include <kdb.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdbrecord.h>

#include <fcntl.h>
#include <stdio.h> // dprintf
#include <sys/file.h>
#include <unistd.h>


int elektraRecorderOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	struct recorderData * p = elektraCalloc (sizeof (struct recorderData));
	p->lockFileFd = -1;
	p->recordingSessionKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	p->recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	const char * lockFilePath = ELEKTRA_RECORDER_DEFAULT_LOCK_FILE_PATH;
	KeySet * config = elektraPluginGetConfig (handle);
	Key * lockFilePathKey = NULL;

	if (config != NULL && (lockFilePathKey = ksLookupByName (config, "/lockfile", 0)) != NULL)
	{
		lockFilePath = keyString (lockFilePathKey);
	}

	p->lockFilePath = elektraCalloc (sizeof (char) * (strlen (lockFilePath) + 1));
	strcpy (p->lockFilePath, lockFilePath);

	elektraPluginSetData (handle, p);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRecorderClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	struct recorderData * p = elektraPluginGetData (handle);
	if (p != NULL)
	{
		keyDel (p->recordingSessionKey);
		keyDel (p->recordingConfigKey);
		elektraFree (p->lockFilePath);
		elektraFree (p);

		elektraPluginSetData (handle, NULL);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRecorderGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/recorder"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/recorder", KEY_VALUE, "recorder plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports", KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/open", KEY_FUNC, elektraRecorderOpen, KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/close", KEY_FUNC, elektraRecorderClose, KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/get", KEY_FUNC, elektraRecorderGet, KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/hook/record/lock", KEY_FUNC, elektraRecorderLock, KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/hook/record/unlock", KEY_FUNC, elektraRecorderUnlock, KEY_END),
			keyNew ("system:/elektra/modules/recorder/exports/hook/record/record", KEY_FUNC, elektraRecorderRecord, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/recorder/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static KDB * getKdb (Plugin * handle, Key * parentKey)
{
	KeySet * global = elektraPluginGetGlobalKeySet (handle);
	Key * kdbKey = ksLookupByName (global, "system:/elektra/kdb", 0);
	if (kdbKey == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Key system:/elektra/kdb was not present in global keyset");
		return NULL;
	}

	const void * kdbPtr = keyValue (kdbKey);
	KDB * hostKdb = kdbPtr == NULL ? NULL : *(KDB **) keyValue (kdbKey);
	if (hostKdb == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "No valid KDB instance found");
		return NULL;
	}

	return hostKdb;
}

int elektraRecorderLock (Plugin * handle, Key * parentKey)
{
	KDB * hostKdb = getKdb (handle, parentKey);
	if (hostKdb == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (!elektraRecordIsActive (hostKdb))
	{
		// If recording is not active -> no need to require lock
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	struct recorderData * recorderData = elektraPluginGetData (handle);

	if (recorderData == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Recorder plugin data is NULL!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (keyIsBelowOrSame (recorderData->recordingSessionKey, parentKey) ||
	    keyIsBelowOrSame (recorderData->recordingConfigKey, parentKey))
	{
		// Don't try to lock, we won't record anything anyway.
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int fd = open (recorderData->lockFilePath, O_CREAT | O_WRONLY, 0666);
	if (fd == -1)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Could not create lockfile %s. Reason: %s", recorderData->lockFilePath,
					     strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int res = flock (fd, LOCK_EX | LOCK_NB);
	if (res == -1)
	{
		close (fd);
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Could not lock file %s. Reason: %s", recorderData->lockFilePath, strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	recorderData->lockFileFd = fd;

	res = ftruncate (fd, 0);
	if (res == -1)
	{
		ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Couldn't truncate lockfile %s. Reason: %s", recorderData->lockFilePath, strerror (errno));
	}

	dprintf (fd, "%d", getpid ());

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRecorderUnlock (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	struct recorderData * recorderData = elektraPluginGetData (handle);

	if (recorderData == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Recorder plugin data is NULL!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (recorderData->lockFileFd != -1)
	{
		// We purposely do not remove the lockfile, as this is not possible without causing a race condition

		// Clear the contents of the lockfile
		int res = ftruncate (recorderData->lockFileFd, 0);
		if (res == -1)
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Couldn't truncate lockfile %s. Reason: %s", recorderData->lockFilePath, strerror (errno));
		}

		// close also removes the lock obtained by flock for this process
		res = close (recorderData->lockFileFd);
		if (res == -1)
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Could not close lockfile %s. Reason: %s", recorderData->lockFilePath,
						       strerror (errno));
		}

		recorderData->lockFileFd = -1;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRecorderRecord (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KDB * hostKdb = getKdb (handle, parentKey);
	if (hostKdb == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (!elektraRecordIsActive (hostKdb))
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * sessionRecordingErrorKey = keyNew ("/", KEY_END);
	KDB * recordingKdb = kdbOpen (NULL, sessionRecordingErrorKey);

	bool success = elektraRecordRecord (hostKdb, recordingKdb, returned, parentKey, parentKey);

	kdbClose (recordingKdb, sessionRecordingErrorKey);
	keyDel (sessionRecordingErrorKey);

	if (!success)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("recorder",
		ELEKTRA_PLUGIN_OPEN,	&elektraRecorderOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraRecorderClose,
		ELEKTRA_PLUGIN_GET,	&elektraRecorderGet,
		ELEKTRA_PLUGIN_END);
}
