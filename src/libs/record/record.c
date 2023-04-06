#include <kdbchangetracking.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdbrecord.h>

/**
 * Enable session recording.
 * This affects both the given @p handle as well as every KDB instance that will be created after this method has been called.
 *
 * @param handle the KDB instance to use
 * @param parentKey recording will be enabled for every key that is same or below this key
 * @param errorKey used for reporting errors and warnings.
 *                 As usual, they will be found as meta keys attached to this key.
 * @retval true - recording has been enabled successfully
 * @retfal false - there was an error enabling recording - see @p errorKey for further details
 */
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

	if (handle->hooks.record.plugin == NULL)
	{
		ELEKTRA_ADD_RESOURCE_WARNING (
			errorKey, "There is no record plugin present. Session recording will not work for the current KDB instance.");
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
		ELEKTRA_ADD_RESOURCE_WARNINGF (
			errorKey, "There is already a session active with parent key \"%s\". Replacing it with new parent key \"%s\".",
			keyString (activeKey), keyName (parentKey));
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

/**
 * Disable session recording.
 * This affects both the given @p handle as well as every KDB instance that will be created after this method has been called.
 *
 * @param handle the KDB instance to use
 * @param errorKey used for reporting errors and warnings.
 *                 As usual, they will be found as meta keys attached to this key.
 * @retval true - recording has been disabled successfully
 * @retfal false - there was an error disabling recording - see @p errorKey for further details
 */
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

/**
 * Clears all recorded data.
 *
 * @param handle the KDB instance to use
 * @param errorKey used for reporting errors and warnings.
 *                 As usual, they will be found as meta keys attached to this key.
 * @retval true - recording session has been cleared successfully
 * @retfal false - there was an error clearing the recording session - see @p errorKey for further details
 */
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

/**
 * Check whether session recording is active in the given KDB instance
 *
 * @param handle the KDB instance to check
 * @retval @p true if session recording is active in @p handle
 * @retval @p false if session recording is not active in @p handle
 * @retval @p false if @p handle is @p NULL
 */
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

/**
 * @internal
 *
 * Replace the @p oldPrefix in all namespaces with @p newPrefix in a given KeySet
 *
 * @param oldPrefix the prefix to remove
 * @param newPrefix the prefix @p oldPrefix should be replaced with
 * @param ks the keyset in which the operation should be carried out
 * @retval @p NULL if any of @p oldPrefix, @p newPrefix or @p ks is @p NULL
 * @retval @p ks if the operation succeeded
 */
static KeySet * renameKeysInAllNamespaces (const char * oldPrefix, const char * newPrefix, KeySet * ks)
{
	if (oldPrefix == NULL || newPrefix == NULL || ks == NULL)
	{
		return NULL;
	}

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

/**
 * @internal
 *
 * Converts a KeySet in session recording format into an @p ElektraDiff.
 *
 * @param recordStorage the KeySet that contains the session diff
 * @param sessionRecordingParentKey the parent key of the recording session
 *
 * @retval an instance of @p ElektraDiff representing the session diff
 * @retval @p NULL if @p recordStorage is @p NULL
 */
static ElektraDiff * getDiffFromSessionStorage (KeySet * recordStorage, Key * sessionRecordingParentKey)
{
	if (recordStorage == NULL)
	{
		return NULL;
	}

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

/**
 * @internal
 *
 * Adds a session storage format representation of the given session diff to the provided KeySet
 *
 * @param recordStorage the KeySet where the keys should be added
 * @param sessionDiff the diff to add
 */
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

/**
 * Diff and record changes to the KDB instance in @p handle.
 * This function is mainly intended for use in the recorder plugin.
 *
 * @param handle the KDB instance that changes occured on
 * @param sessionStorageHandle the KDB instances that shall be used to persist the session diff.
 *                             You can use the same as for @p handle.
 * @param newKeys the keyset with changed keys
 * @param parentKey only changes same or below this key will be determined
 * @param errorKey used for reporting errors and warnings.
 *                 As usual, they will be found as meta keys attached to this key.
 * @retval true - changes were recorded successfully
 * @retfal false - there was an error during recording - see @p errorKey for further details
 */
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
		ELEKTRA_ADD_RESOURCE_WARNINGF (errorKey, "%s called but recording is not enabled.", __func__);
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

/**
 * Undo changes that were recorded in the current recording session.
 * After executing this function, the state of KDB should be the same as it was before starting the recording session.
 *
 * @param handle the KDB instance to use for accessing configuration data
 * @param sessionStorageHandle the KDB instance to use for accessing recording data.
 *                             This can be the same as for @p handle
 * @param parentKey only changes same or below this key are undone.
 * @param errorKey used for reporting errors and warnings.
 *                 As usual, they will be found as meta keys attached to this key.
 * @retval true - changes were undone successfully
 * @retfal false - there was an error during the undo operation - see @p errorKey for further details
 */
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

static KeySet * buildExportKeySet (ElektraDiff * diff)
{
	KeySet * ks = ksNew (0, KS_END);

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	ksAppend (ks, addedKeys);
	ksDel (addedKeys);

	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);
	ksAppend (ks, modifiedKeys);
	ksDel (addedKeys);

	KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);
	for (elektraCursor i = 0; i < ksGetSize (removedKeys); i++)
	{
		Key * k = ksAtCursor (removedKeys, i);
		keySetMeta (k, "meta:/elektra/deleted", "true");
	}
	ksAppend (ks, removedKeys);
	ksDel (removedKeys);

	return ks;
}

bool elektraRecordExportSession (KDB * handle, Plugin * plugin, Key * parentKey, Key * errorKey)
{
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for KDB handle");
		return false;
	}

	if (plugin == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL pointer passed for plugin");
		return false;
	}

	Key * sessionKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	if (kdbGet (handle, sessionStorage, sessionKey) == -1)
	{
		elektraCopyError (errorKey, sessionKey);
		keyDel (sessionKey);
		ksDel (sessionStorage);
		return false;
	}

	ElektraDiff * sessionDiff = getDiffFromSessionStorage (sessionStorage, parentKey);
	KeySet * exportKs = buildExportKeySet (sessionDiff);
	int result = plugin->kdbSet (plugin, exportKs, parentKey);
	bool successful = true;
	if (result == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		elektraCopyError (errorKey, parentKey);
		successful = false;
	}

	elektraDiffDel (sessionDiff);
	keyDel (parentKey);
	keyDel (sessionKey);
	ksDel (sessionStorage);
	ksDel (exportKs);

	return successful;
}
