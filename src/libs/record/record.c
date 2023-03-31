#include <kdbchangetracking.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdbrecord.h>

void elektraRecordEnableRecording (KDB * handle, const Key * parentKey, Key * errorKey)
{
	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	kdbGet (handle, config, configKey);

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

	kdbSet (handle, config, configKey);

	ksAppendKey (handle->global, activeKey);

	keyDel (configKey);
	ksDel (config);
}

void elektraRecordDisableRecording (KDB * handle, Key * errorKey)
{
	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	kdbGet (handle, config, configKey);

	Key * activeKey = NULL;
	while ((activeKey = ksLookupByName (config, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	kdbSet (handle, config, configKey);

	while ((activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	keyDel (configKey);
	ksDel (config);
}

void elektraRecordClearSession (KDB * handle, Key * errorKey)
{
	Key * sessionKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	KeySet * session = ksNew (0, KS_END);

	kdbGet (handle, session, sessionKey);
	ksDel (ksCut (session, sessionKey));
	kdbSet (handle, session, sessionKey);

	keyDel (sessionKey);
	ksDel (session);
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
	const Key * activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0);
	if (activeKey == NULL)
	{
		ELEKTRA_ADD_INTERNAL_WARNINGF (errorKey, "Key %s was not found", ELEKTRA_RECORD_CONFIG_ACTIVE_KEY);
		return false;
	}

	Key * recordConfigurationKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);
	Key * sessionRecordingKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	if (keyIsBelowOrSame (sessionRecordingKey, parentKey) || keyIsBelowOrSame (recordConfigurationKey, parentKey))
	{
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		return true;
	}

	KeySet * duped = ksDup (newKeys);
	ksDel (ksCut (duped, sessionRecordingKey));
	ksDel (ksCut (duped, recordConfigurationKey));

	if (ksGetSize (duped) == 0 && ksGetSize (newKeys) != 0)
	{
		keyDel (sessionRecordingKey);
		keyDel (recordConfigurationKey);
		ksDel (duped);
		return true;
	}

	Key * sessionRecordingParentKey = keyNew (keyString (activeKey), KEY_END);

	const ChangeTrackingContext * changeTrackingContext = elektraChangeTrackingGetContextFromKdb (handle);
	ElektraDiff * partDiff = elektraChangeTrackingCalculateDiff (duped, changeTrackingContext, parentKey);
	elektraDiffRemoveSameOrBelow (partDiff, sessionRecordingKey);
	elektraDiffRemoveSameOrBelow (partDiff, recordConfigurationKey);

	if (!elektraDiffIsEmpty (partDiff))
	{
		KeySet * recordStorage = ksNew (0, KS_END);
		kdbGet (sessionStorageHandle, recordStorage, sessionRecordingKey);

		ElektraDiff * sessionDiff = getDiffFromSessionStorage (recordStorage, sessionRecordingParentKey);
		Key * appendKey = keyNew ("/", KEY_END);
		elektraDiffAppend (sessionDiff, partDiff, appendKey);
		keyDel (appendKey);

		putDiffIntoSessionStorage (recordStorage, sessionDiff);

		kdbSet (sessionStorageHandle, recordStorage, sessionRecordingKey);

		elektraDiffDel (sessionDiff);
		ksDel (recordStorage);
	}

	keyDel (sessionRecordingKey);
	keyDel (recordConfigurationKey);
	keyDel (sessionRecordingParentKey);
	ksDel (duped);
	elektraDiffDel (partDiff);

	return true;
}

bool elektraRecordUndo (KDB * handle, KDB * sessionStorageHandle, Key * parentKey, Key * errorKey)
{
	Key * sessionRecordingKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	KeySet * recordStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageHandle, recordStorage, sessionRecordingKey);

	ElektraDiff * sessionDiff = getDiffFromSessionStorage (recordStorage, NULL);
	ElektraDiff * undoDiff = elektraDiffCut (sessionDiff, parentKey);

	if (!elektraDiffIsEmpty (undoDiff))
	{
		KeySet * ks = ksNew (0, KS_END);
		kdbGet (handle, ks, parentKey);

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

		kdbSet (handle, ks, parentKey);

		putDiffIntoSessionStorage (recordStorage, sessionDiff);
		kdbSet (sessionStorageHandle, recordStorage, sessionRecordingKey);

		if (activeKey != NULL)
		{
			// Reenable session recording
			ksAppendKey (handle->global, activeKey);
		}

		ksDel (keysToRemove);
		ksDel (keysToModify);
		ksDel (keysToAdd);
		ksDel (ks);
	}

	keyDel (sessionRecordingKey);
	ksDel (recordStorage);

	elektraDiffDel (sessionDiff);
	elektraDiffDel (undoDiff);

	return true;
}
