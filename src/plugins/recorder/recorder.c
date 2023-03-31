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
#include <kdbrecord.h>

int elektraRecorderOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRecorderClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

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

int elektraRecorderRecord (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KeySet * global = elektraPluginGetGlobalKeySet (handle);
	Key * kdbKey = ksLookupByName (global, "system:/elektra/kdb", 0);
	if (kdbKey == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Keys system:/elektra/kdb was not present in global keyset");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const void * kdbPtr = keyValue (kdbKey);
	KDB * hostKdb = kdbPtr == NULL ? NULL : *(KDB **) keyValue (kdbKey);
	if (hostKdb == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "No valid KDB instance found");
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
