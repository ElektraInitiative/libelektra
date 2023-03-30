/**
 * @file
 *
 * @brief Source for recorder plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "recorder.h"

#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdbchangetracking.h>
#include <stdio.h>

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
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/recorder", KEY_VALUE, "recorder plugin waits for your orders", KEY_END),
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

static KeySet * renameKeysInAllNamespaces (const char * oldPrefix, const char * newPrefix, KeySet * ks)
{
	Key * prefixKey = keyNew (oldPrefix, KEY_END);
	Key * newRootKey = keyNew (newPrefix, KEY_END);

	for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ns++)
	{
		keySetNamespace (prefixKey, ns);
		keySetNamespace (newRootKey, ns);
		ksRename (ks, prefixKey, newRootKey);
	}

	keyDel (prefixKey);
	keyDel (newRootKey);

	return ks;
}

static ElektraDiff * getDiffFromSessionStorage (KeySet * recordStorage, Key * sessionRecordingParentKey)
{
	Key * sessionDiffAddedKey = keyNew (ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY, KEY_END);
	Key * sessionDiffModifiedKey = keyNew (ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY, KEY_END);
	Key * sessionDiffRemovedKey = keyNew (ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY, KEY_END);

	ElektraDiff * sessionDiff = elektraDiffNew (
		renameKeysInAllNamespaces (ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY, "/", ksCut (recordStorage, sessionDiffAddedKey)),
		renameKeysInAllNamespaces (ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY, "/", ksCut (recordStorage, sessionDiffRemovedKey)),
		renameKeysInAllNamespaces (ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY, "/", ksCut (recordStorage, sessionDiffModifiedKey)),
						    NULL, /* we don't need new keys */
						    sessionRecordingParentKey);

	keyDel (sessionDiffAddedKey);
	keyDel (sessionDiffModifiedKey);
	keyDel (sessionDiffRemovedKey);

	return sessionDiff;
}

static void putDiffIntoSessionStorage (KeySet * recordStorage, ElektraDiff * sessionDiff)
{
	KeySet * addedKeys = renameKeysInAllNamespaces ("/", ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY, elektraDiffGetAddedKeys (sessionDiff));
	KeySet * modifiedKeys = renameKeysInAllNamespaces ("/", ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY, elektraDiffGetModifiedKeys (sessionDiff));
	KeySet * removedKeys = renameKeysInAllNamespaces ("/", ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY, elektraDiffGetRemovedKeys (sessionDiff));

	ksAppend (recordStorage, addedKeys);
	ksAppend (recordStorage, modifiedKeys);
	ksAppend (recordStorage, removedKeys);

	ksDel (addedKeys);
	ksDel (modifiedKeys);
	ksDel (removedKeys);
}

int elektraRecorderRecord (Plugin * handle, KeySet * returned, Key * parentKey)
{
	const Key * activeKey = ksLookupByName (elektraPluginGetGlobalKeySet (handle), ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0);
	if (activeKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * recordConfigurationKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);
	Key * sessionRecordingKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	if (keyIsBelowOrSame (sessionRecordingKey, parentKey) || keyIsBelowOrSame (recordConfigurationKey, parentKey))
	{
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	KeySet * duped = ksDup (returned);
	ksDel (ksCut (duped, sessionRecordingKey));
	ksDel (ksCut (duped, recordConfigurationKey));

	if (ksGetSize (duped) == 0 && ksGetSize (returned) != 0)
	{
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		ksDel (duped);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * sessionRecordingParentKey = keyNew (keyString(activeKey), KEY_END);

	const ChangeTrackingContext * changeTrackingContext = elektraChangeTrackingGetContextFromPlugin (handle);
	ElektraDiff * partDiff = elektraChangeTrackingCalculateDiff (duped, changeTrackingContext, parentKey);
	elektraDiffRemoveSameOrBelow (partDiff, sessionRecordingKey);
	elektraDiffRemoveSameOrBelow (partDiff, recordConfigurationKey);

	KDB * recordingKdb = kdbOpen (NULL, sessionRecordingKey);

	KeySet * recordStorage = ksNew (0, KS_END);
	kdbGet (recordingKdb, recordStorage, sessionRecordingKey);

	ElektraDiff * sessionDiff = getDiffFromSessionStorage (recordStorage, sessionRecordingParentKey);
	Key * appendKey = keyNew ("/", KEY_END);
	elektraDiffAppend (sessionDiff, partDiff, appendKey);
	keyDel (appendKey);

	putDiffIntoSessionStorage (recordStorage, sessionDiff);

	kdbSet (recordingKdb, recordStorage, sessionRecordingKey);

	kdbClose (recordingKdb, sessionRecordingKey);


	keyDel (sessionRecordingKey);
	keyDel (recordConfigurationKey);
	keyDel (sessionRecordingParentKey);
	ksDel (duped);
	ksDel (recordStorage);
	elektraDiffDel (partDiff);
	elektraDiffDel (sessionDiff);

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
