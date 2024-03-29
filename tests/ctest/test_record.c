#include <kdbprivate.h>
#include <kdbrecord.h>
#include <stdio.h>
#include <tests.h>

#define printErrorAndClear(key) logKeyErrorAndClear (key, __FILE__, __LINE__)

static void logKeyErrorAndClear (Key * key, char * file, int line)
{
	if (key == NULL)
	{
		return;
	}

	if (keyGetMeta (key, "meta:/error") == NULL)
	{
		return;
	}

	printf ("Caught error in %s:%d!\n", file, line);

	const Key * m = NULL;
	if ((m = keyGetMeta (key, "meta:/error/number")) != NULL)
	{
		printf ("error/number = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/description")) != NULL)
	{
		printf ("error/description = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/module")) != NULL)
	{
		printf ("error/module = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/file")) != NULL)
	{
		printf ("error/file = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/line")) != NULL)
	{
		printf ("error/line = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/mountpoint")) != NULL)
	{
		printf ("error/mountpoint = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/configfile")) != NULL)
	{
		printf ("error/configfile = %s\n", keyString (m));
		fflush (stdout);
	}
	if ((m = keyGetMeta (key, "meta:/error/reason")) != NULL)
	{
		printf ("error/reason = %s\n", keyString (m));
		fflush (stdout);
	}

	Key * metaRoot = keyNew ("meta:/error", KEY_END);
	ksDel (ksCut (keyMeta (key), metaRoot));
	keyDel (metaRoot);
}

static char * appendString (const char * s1, const char * s2)
{
	size_t newPathLength = strlen (s1) + strlen (s2) + 1;
	char * newPath = elektraMalloc (newPathLength * sizeof (char));
	snprintf (newPath, newPathLength, "%s%s", s1, s2);
	return newPath;
}

static KDB * openPrefixedKdbInstance (KeySet * contract, Key * errorKey, const char * prefix)
{
	char * bootstrapPath = appendString (prefix, KDB_DB_INIT);
	elektraBootstrapPathContract (contract, bootstrapPath);
	KDB * kdb = kdbOpen (contract, errorKey);
	free (bootstrapPath);

	// TODO: use new C mount library to modify mountpoints like we do in the C++ gtest framework
	for (elektraCursor i = 0; i < ksGetSize (kdb->backends); i++)
	{
		Key * backendKey = ksAtCursor (kdb->backends, i);
		BackendData * backendData = (BackendData *) keyValue (backendKey);

		Key * pathKey = ksLookupByName (backendData->definition, "system:/path", 0);
		if (pathKey == NULL)
		{
			continue;
		}

		const char * oldPath = keyString (pathKey);
		char * newPath = appendString (prefix, oldPath);
		keySetString (pathKey, newPath);
		elektraFree (newPath);
	}

	return kdb;
}

static void closePrefixedKdbInstance (KDB * kdb, Key * errorKey, bool deleteFiles)
{
	// remove all files
	if (deleteFiles)
	{
		KeySet * tmp = ksNew (0, KS_END);
		Key * pk = keyNew ("/", KEY_END);

		kdbGet (kdb, tmp, pk);
		ksClear (tmp);
		kdbSet (kdb, tmp, pk);

		ksDel (tmp);
		keyDel (pk);
	}

	kdbClose (kdb, errorKey);
}

static void test_elektraRecordIsActive_onEmptyKdb (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	bool isActive = elektraRecordIsActive (kdb);

	// Assert
	succeed_if (isActive == false, "should be inactive on empty instance");

	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);

	ksDel (contract);
	keyDel (parentKey);
}

static void test_elektraRecordEnableRecording_isActive_Disable_shouldWork (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	Key * recordingParentKey = keyNew ("user:/my/test", KEY_END);

	// Enable
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	bool success = elektraRecordEnableRecording (kdb, recordingParentKey, parentKey);
	succeed_if (success == true, "call should be successful");
	printErrorAndClear (parentKey);
	succeed_if_keyset_contains_key_with_string (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);
	KeySet * tmp = ksNew (0, KS_END);
	Key * recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	printErrorAndClear (recordingConfigKey);
	succeed_if_keyset_contains_key_with_string (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	succeed_if_keyset_contains_key_with_string (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == true, "elektraRecordIsActive should report true");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	// Create new instance and disable
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);
	success = elektraRecordDisableRecording (kdb, recordingParentKey);
	printErrorAndClear (recordingParentKey);

	succeed_if (success == true, "call should be successful");
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL,
		    "global keyset should not contain active key");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);
	tmp = ksNew (0, KS_END);
	recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	printErrorAndClear (recordingConfigKey);
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL,
		    "global keyset should not contain active key");
	succeed_if (ksLookupByName (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL, "should not contain active key");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == false, "elektraRecordIsActive should report false");
	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);


	ksDel (contract);
	keyDel (parentKey);
	keyDel (recordingParentKey);
}

static void test_elektraRecordRecord_notActive_shouldAddWarningIfNotActive (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);
	Key * errorKey = keyNew ("/", KEY_END);

	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:/test", KEY_VALUE, "123", KEY_END));

	// Act
	bool success = elektraRecordRecord (kdb, kdb, keys, parentKey, errorKey);
	printErrorAndClear (errorKey);

	// Assert
	succeed_if (success == true, "should return successful status code");
	succeed_if (keyGetMeta (errorKey, "meta:/warnings/#0") != NULL, "should have a warning set");

	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);

	ksDel (contract);
	ksDel (keys);
	keyDel (parentKey);
	keyDel (errorKey);
}

static void test_elektraRecordRecord_shouldRecordChanges (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	elektraRecordEnableRecording (kdb, parentKey, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	keyDel (ksLookupByName (keys, "user:/test/key1", KDB_O_POP));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key3", KEY_VALUE, "2", KEY_END));

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	printErrorAndClear (parentKey);

	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 4, "expected 4 keys, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key3", "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key1", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", "2");

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);
	printErrorAndClear (parentKey);

	keyDel (parentKey);
	keyDel (errorKey);
}

static void test_elektraRecordRecord_withParentKeySet_shouldRecordOnlyChangesBelowParentKey (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/filter/filterkey1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	Key * recordingParentKey = keyNew ("user:/filter", KEY_END);
	elektraRecordEnableRecording (kdb, recordingParentKey, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	keyDel (ksLookupByName (keys, "user:/test/key1", KDB_O_POP));
	keyDel (ksLookupByName (keys, "user:/filter/filterkey1", KDB_O_POP));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key3", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/filter/filterkey2", KEY_VALUE, "2", KEY_END));

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	printErrorAndClear (parentKey);

	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 2, "expected 3 keys, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/filter/filterkey2",
						    "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/filter/filterkey1",
						    "1");

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);
	printErrorAndClear (parentKey);

	keyDel (parentKey);
	keyDel (errorKey);
	keyDel (recordingParentKey);
}

static void test_elektraRecordRecord_removeEverything_shouldRecordChanges (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	elektraRecordEnableRecording (kdb, parentKey, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksClear (keys);

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	printErrorAndClear (parentKey);

	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 2, "expected 2 keys, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key1", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key2", "1");

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (kdb, parentKey, true);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);
	printErrorAndClear (parentKey);

	keyDel (parentKey);
	keyDel (errorKey);
}

static void test_elektraRecordUndo_shouldUndoChanges (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));

	// We also need to store the session diff right here
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	bool success = elektraRecordUndo (kdb, sessionStorageKdb, parentKey, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	printErrorAndClear (parentKey);

	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 0, "expected 0 keys in session storage, was %zu", ksGetSize (sessionStorage));

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	keyDel (parentKey);
	parentKey = keyNew ("user:/", KEY_END);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	ksClear (keys);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	succeed_if_keyset_contains_key_with_string (keys, "user:/test/key3", "3");
	succeed_if_keyset_contains_key_with_string (keys, "user:/test/key2", "1");
	succeed_if (ksLookupByName (keys, "user:/test/key1", 0) == NULL, "user:/test/key1 should not have been found");

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);
	printErrorAndClear (parentKey);

	keyDel (parentKey);
	keyDel (errorKey);
}

static void test_elektraRecordRemoveKey_nonRecursive_shouldRemoveKeyFromSession (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3/sub", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2/sub", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2/sub", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	Key * errorKey = keyNew ("/", KEY_END);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	KeySet * toRemove = ksNew (1, keyNew ("user:/test/key3", KEY_END), KS_END);
	bool success = elektraRecordRemoveKeys (kdb, toRemove, false, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 6, "expected 4 keys in session storage, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3/sub", "3");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2/sub",
						    "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2/sub",
						    "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", "1");

	closePrefixedKdbInstance (sessionStorageKdb, sessionStorageKey, false);
	printErrorAndClear (sessionStorageKey);

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	keyDel (parentKey);
	keyDel (errorKey);

	ksDel (toRemove);
	ksDel (contract);
	ksDel (keys);
}

static void test_elektraRecordRemoveKey_recursive_shouldRemoveKeyAndBelowFromSession (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3/sub", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2/sub", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2/sub", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	Key * errorKey = keyNew ("/", KEY_END);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	KeySet * toRemove = ksNew (1, keyNew ("user:/test/key3", KEY_END), KS_END);
	bool success = elektraRecordRemoveKeys (kdb, toRemove, true, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);
	printErrorAndClear (sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 5, "expected 5 keys in session storage, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2/sub",
						    "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2/sub",
						    "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", "1");

	closePrefixedKdbInstance (sessionStorageKdb, sessionStorageKey, false);
	printErrorAndClear (sessionStorageKey);

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	keyDel (parentKey);
	keyDel (errorKey);

	ksDel (toRemove);
	ksDel (contract);
	ksDel (keys);
}

static void test_elektraRecordGetDiff_shouldProvideDiff (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/", KEY_END);

	// Fill some keys into kdb before recording
	KDB * kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_OLD_KEY "/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_NEW_KEY "/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	printErrorAndClear (parentKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);

	Key * errorKey = keyNew ("/", KEY_END);
	printErrorAndClear (parentKey);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printErrorAndClear (parentKey);

	// Act
	ElektraDiff * diff = NULL;
	bool success = elektraRecordGetDiff (kdb, &diff, errorKey);
	printErrorAndClear (errorKey);

	closePrefixedKdbInstance (kdb, parentKey, false);
	printErrorAndClear (parentKey);


	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	succeed_if (diff != NULL, "diff should not be NULL");

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);
	KeySet * modifiedNewKeys = elektraDiffGetModifiedNewKeys (diff);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);

	succeed_if_fmt (ksGetSize (addedKeys) == 1, "added keys should contain 1 key, but was %zu", ksGetSize (addedKeys));
	succeed_if_keyset_contains_key_with_string (addedKeys, "user:/test/key1", "1");

	succeed_if_fmt (ksGetSize (modifiedKeys) == 1, "modified keys should contain 1 key, but was %zu", ksGetSize (modifiedKeys));
	succeed_if_keyset_contains_key_with_string (modifiedKeys, "user:/test/key2", "1");

	succeed_if_fmt (ksGetSize (modifiedNewKeys) == 1, "modified new keys should contain 1 key, but was %zu", ksGetSize (modifiedKeys));
	succeed_if_keyset_contains_key_with_string (modifiedNewKeys, "user:/test/key2", "2");

	succeed_if_fmt (ksGetSize (removedKeys) == 1, "removed keys should contain 1 key, but was %zu", ksGetSize (removedKeys));
	succeed_if_keyset_contains_key_with_string (removedKeys, "user:/test/key3", "3");

	elektraDiffDel (diff);

	ksDel (addedKeys);
	ksDel (modifiedKeys);
	ksDel (modifiedNewKeys);
	ksDel (removedKeys);

	keyDel (parentKey);
	keyDel (errorKey);

	ksDel (contract);
	ksDel (keys);
}

int main (int argc, char ** argv)
{
	printf ("RECORD       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraRecordIsActive_onEmptyKdb ();
	test_elektraRecordEnableRecording_isActive_Disable_shouldWork ();
	test_elektraRecordRecord_notActive_shouldAddWarningIfNotActive ();
	test_elektraRecordRecord_shouldRecordChanges ();
	test_elektraRecordRecord_withParentKeySet_shouldRecordOnlyChangesBelowParentKey ();
	test_elektraRecordRecord_removeEverything_shouldRecordChanges ();
	test_elektraRecordUndo_shouldUndoChanges ();
	test_elektraRecordRemoveKey_nonRecursive_shouldRemoveKeyFromSession ();
	test_elektraRecordRemoveKey_recursive_shouldRemoveKeyAndBelowFromSession ();
	test_elektraRecordGetDiff_shouldProvideDiff ();

	printf ("\ntest_record RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	fflush (stdout);

	return nbError;
}
