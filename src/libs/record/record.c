#include <kdbchangetracking.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdbrecord.h>

bool elektraRecordEnableRecording (KDB * handle, const Key * parentKey, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	if (parentKey == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for parent key");
		return false;
	}

	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	if (kdbGet (handle, config, configKey) == -1)
	{
		elektraCopyError (errorKey, configKey);
		keyDel (configKey);
		ksDel (config);
		return false;
	}

	elektraNamespace ns = KEY_NS_SYSTEM;
	Key * activeKey = ksLookupByName (config, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP);
	if (activeKey != NULL)
	{
		ns = keyGetNamespace (activeKey);
		keyDel (activeKey);
	}

	activeKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_VALUE, keyName (parentKey), KEY_END);
	keySetNamespace (activeKey, ns);

	ksAppendKey (config, activeKey);

	if (kdbSet (handle, config, configKey) == -1)
	{
		elektraCopyError (errorKey, configKey);
		keyDel (configKey);
		ksDel (config);
		keyDel (activeKey);
		return false;
	}

	ksAppendKey (handle->global, activeKey);

	keyDel (configKey);
	ksDel (config);

	return true;
}

bool elektraRecordDisableRecording (KDB * handle, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	if (kdbGet (handle, config, configKey) == -1)
	{
		elektraCopyError (errorKey, configKey);
		keyDel (configKey);
		ksDel (config);
		return false;
	}

	Key * activeKey = NULL;
	while ((activeKey = ksLookupByName (config, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	if (kdbSet (handle, config, configKey) == -1)
	{
		elektraCopyError (errorKey, configKey);
		keyDel (configKey);
		ksDel (config);
		keyDel (activeKey);
		return false;
	}

	while ((activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	keyDel (configKey);
	ksDel (config);

	return true;
}

bool elektraRecordClearSession (KDB * handle, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	Key * sessionKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * session = ksNew (0, KS_END);

	if (kdbGet (handle, session, sessionKey) == -1)
	{
		elektraCopyError (errorKey, sessionKey);
		keyDel (sessionKey);
		ksDel (session);
		return false;
	}

	ksDel (ksCut (session, sessionKey));

	if (kdbSet (handle, session, sessionKey) == -1)
	{
		elektraCopyError (errorKey, sessionKey);
		keyDel (sessionKey);
		ksDel (session);
		return false;
	}

	keyDel (sessionKey);
	ksDel (session);

	return true;
}

bool elektraRecordIsActive (KDB * handle)
{
	if (handle == NULL)
	{
		return false;
	}

	const Key * activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0);
	if (activeKey == NULL)
	{
		return false;
	}

	return true;
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
	KeySet * modifiedKeys =
		renameKeysInAllNamespaces ("/", ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY, elektraDiffGetModifiedKeys (sessionDiff));
	KeySet * removedKeys =
		renameKeysInAllNamespaces ("/", ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY, elektraDiffGetRemovedKeys (sessionDiff));

	ksAppend (recordStorage, addedKeys);
	ksAppend (recordStorage, modifiedKeys);
	ksAppend (recordStorage, removedKeys);

	ksDel (addedKeys);
	ksDel (modifiedKeys);
	ksDel (removedKeys);
}

bool elektraRecordRecord (KDB * handle, KDB * sessionStorageHandle, KeySet * newKeys, Key * parentKey, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	if (sessionStorageHandle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB session storage handle");
		return false;
	}

	if (newKeys == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for new keys");
		return false;
	}

	if (parentKey == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for parent key");
		return false;
	}

	const Key * activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0);
	if (activeKey == NULL)
	{
		// recording is not activated --> do nothing, but still successful
		return true;
	}

	Key * recordConfigurationKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);
	Key * sessionRecordingKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	if (keyIsBelowOrSame (sessionRecordingKey, parentKey) || keyIsBelowOrSame (recordConfigurationKey, parentKey))
	{
		// the parent key is either below our session storage or below our configuration storage
		// we do not want to record changes to the recording tool itself
		// do nothing, but still successful
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		return true;
	}

	// Remove all keys that belong to the recording tool
	KeySet * toRecord = ksDup (newKeys);
	ksDel (ksCut (toRecord, sessionRecordingKey));
	ksDel (ksCut (toRecord, recordConfigurationKey));

	if (ksGetSize (toRecord) == 0 && ksGetSize (newKeys) != 0)
	{
		// after clearing out all keys that belong to the recording tool there are no more keys
		// do nothing, but still successful
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		ksDel (toRecord);
		return true;
	}

	const ChangeTrackingContext * changeTrackingContext = elektraChangeTrackingGetContextFromKdb (handle);
	if (changeTrackingContext == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (errorKey, "Could not get changetracking context from KDB");
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		ksDel (toRecord);
		return false;
	}

	// Root key for which session recording is enabled
	Key * sessionRecordingParentKey = keyNew (keyString (activeKey), KEY_END);

	// Parent key for the part diff
	Key * parentKeyForDiff = parentKey;
	if (keyIsBelow (parentKey, sessionRecordingParentKey))
	{
		// if the sessionRecordingParentKey is below parentKey we only need to diff changes below sessionRecordingParentKey
		parentKeyForDiff = sessionRecordingParentKey;
	}

	ElektraDiff * partDiff = elektraChangeTrackingCalculateDiff (toRecord, changeTrackingContext, parentKeyForDiff);
	elektraDiffRemoveSameOrBelow (partDiff, sessionRecordingKey);
	elektraDiffRemoveSameOrBelow (partDiff, recordConfigurationKey);

	bool successful = true;
	if (!elektraDiffIsEmpty (partDiff))
	{
		KeySet * recordStorage = ksNew (0, KS_END);

		// load data for current session diff
		if (kdbGet (sessionStorageHandle, recordStorage, sessionRecordingKey) == -1)
		{
			elektraCopyError (errorKey, sessionRecordingKey);
			successful = false;
			ksDel (recordStorage);
			goto cleanup;
		}

		ElektraDiff * sessionDiff = getDiffFromSessionStorage (recordStorage, sessionRecordingParentKey);

		// Calculate new session diff
		Key * appendKey = keyNew ("/", KEY_END);
		elektraDiffAppend (sessionDiff, partDiff, appendKey);
		keyDel (appendKey);

		putDiffIntoSessionStorage (recordStorage, sessionDiff);

		// store data for session diff
		if (kdbSet (sessionStorageHandle, recordStorage, sessionRecordingKey) == -1)
		{
			elektraCopyError (errorKey, sessionRecordingKey);
			successful = false;
			elektraDiffDel (sessionDiff);
			ksDel (recordStorage);
			goto cleanup;
		}

		elektraDiffDel (sessionDiff);
		ksDel (recordStorage);
	}

cleanup:
	keyDel (sessionRecordingKey);
	keyDel (recordConfigurationKey);
	keyDel (sessionRecordingParentKey);
	ksDel (toRecord);
	elektraDiffDel (partDiff);

	return successful;
}

bool elektraRecordUndo (KDB * handle, KDB * sessionStorageHandle, Key * parentKey, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	if (sessionStorageHandle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB session storage handle");
		return false;
	}

	if (parentKey == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for parent key");
		return false;
	}

	Key * sessionRecordingKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * recordStorage = ksNew (0, KS_END);

	// Load data from session diff
	if (kdbGet (sessionStorageHandle, recordStorage, sessionRecordingKey) == -1)
	{
		elektraCopyError (errorKey, sessionRecordingKey);
		keyDel (sessionRecordingKey);
		ksDel (recordStorage);
		return false;
	}

	ElektraDiff * sessionDiff = getDiffFromSessionStorage (recordStorage, NULL);
	ElektraDiff * undoDiff = elektraDiffCut (sessionDiff, parentKey);

	bool successful = true;
	if (!elektraDiffIsEmpty (undoDiff))
	{
		KeySet * ks = ksNew (0, KS_END);
		if (kdbGet (handle, ks, parentKey) == -1)
		{
			elektraCopyError (errorKey, parentKey);
			successful = false;
			ksDel (ks);
			goto cleanup;
		}

		KeySet * keysToRemove = elektraDiffGetAddedKeys (undoDiff);
		KeySet * keysToModify = elektraDiffGetModifiedKeys (undoDiff);
		KeySet * keysToAdd = elektraDiffGetRemovedKeys (undoDiff);

		for (elektraCursor i = 0; i < ksGetSize (keysToRemove); i++)
		{
			Key * toRemove = ksAtCursor (keysToRemove, i);
			Key * key = ksLookup (ks, toRemove, KDB_O_POP);
			keyDel (key);
		}

		ksAppend (ks, keysToModify);
		ksAppend (ks, keysToAdd);

		// Disable session recording for now
		Key * activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP);

		if (kdbSet (handle, ks, parentKey) == -1)
		{
			elektraCopyError (errorKey, parentKey);
			successful = false;
			goto innercleanup;
		}

		putDiffIntoSessionStorage (recordStorage, sessionDiff);
		if (kdbSet (sessionStorageHandle, recordStorage, sessionRecordingKey) == -1)
		{
			elektraCopyError (errorKey, parentKey);
			successful = false;
			goto innercleanup;
		}

	innercleanup:
		if (activeKey != NULL)
		{
			// Reenable session recording
			ksAppendKey (handle->global, activeKey);
		}

		ksDel (keysToRemove);
		ksDel (keysToModify);
		ksDel (keysToAdd);
		ksDel (ks);

		if (!successful)
		{
			goto cleanup;
		}
	}

cleanup:
	keyDel (sessionRecordingKey);
	ksDel (recordStorage);

	elektraDiffDel (sessionDiff);
	elektraDiffDel (undoDiff);

	return successful;
}
