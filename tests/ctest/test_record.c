#include <kdbprivate.h>
#include <kdbrecord.h>
#include <stdio.h>
#include <tests.h>

static void printError (Key * key)
{
	if (key == NULL)
	{
		return;
	}

	if (keyGetMeta (key, "meta:/error") == NULL)
	{
		return;
	}

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
		ElektraKdbPhase phase = ELEKTRA_KDB_GET_PHASE_RESOLVER;
		ksAppendKey (kdb->global, keyNew ("system:/elektra/kdb/backend/phase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase),
						  KEY_VALUE, &phase, KEY_END));
		Key * pk = keyNew ("/", KEY_END);

		for (elektraCursor i = 0; i < ksGetSize (kdb->backends); i++)
		{
			Key * backendKey = ksAtCursor (kdb->backends, i);

			if (keyGetNamespace (backendKey) == KEY_NS_PROC)
			{
				// proc:/ backends only run in poststorage
				continue;
			}

			keyCopy (pk, backendKey, KEY_CP_NAME);
			BackendData * backendData = (BackendData *) keyValue (backendKey);

			backendData->backend->kdbGet (backendData->backend, tmp, pk);

			const char * path = keyString (pk);
			if (path != NULL && strcmp ("", path) != 0)
			{
				elektraUnlink (path);
			}
		}

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

	// Act
	bool isActive = elektraRecordIsActive (kdb);

	// Assert
	succeed_if (isActive == false, "should be inactive on empty instance");

	closePrefixedKdbInstance (kdb, parentKey, true);
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

	bool success = elektraRecordEnableRecording (kdb, recordingParentKey, parentKey);
	succeed_if (success == true, "call should be successful");
	printError (parentKey);
	succeed_if_keyset_contains_key_with_string (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printError (parentKey);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printError (parentKey);
	KeySet * tmp = ksNew (0, KS_END);
	Key * recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	printError (recordingConfigKey);
	succeed_if_keyset_contains_key_with_string (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	succeed_if_keyset_contains_key_with_string (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == true, "elektraRecordIsActive should report true");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printError (parentKey);

	// Create new instance and disable
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printError (parentKey);
	success = elektraRecordDisableRecording (kdb, recordingParentKey);
	printError (recordingParentKey);

	succeed_if (success == true, "call should be successful");
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL,
		    "global keyset should not contain active key");
	closePrefixedKdbInstance (kdb, parentKey, false);
	printError (parentKey);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	printError (parentKey);
	tmp = ksNew (0, KS_END);
	recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	printError (recordingConfigKey);
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL,
		    "global keyset should not contain active key");
	succeed_if (ksLookupByName (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL, "should not contain active key");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == false, "elektraRecordIsActive should report false");
	closePrefixedKdbInstance (kdb, parentKey, true);
	printError (parentKey);


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
	printError (parentKey);

	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	printError (parentKey);

	ksAppendKey (keys, keyNew ("user:/test", KEY_VALUE, "123", KEY_END));

	// Act
	bool success = elektraRecordRecord (kdb, kdb, keys, parentKey, errorKey);
	printError (errorKey);

	// Assert
	succeed_if (success == true, "should return successful status code");
	succeed_if (keyGetMeta (errorKey, "meta:/warnings/#0") != NULL, "should have a warning set");

	closePrefixedKdbInstance (kdb, parentKey, true);
	printError (parentKey);

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
	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));
	kdbSet (kdb, keys, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	elektraRecordEnableRecording (kdb, parentKey, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);
	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	kdbGet (kdb, keys, parentKey);
	keyDel (ksLookupByName (keys, "user:/test/key1", KDB_O_POP));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key3", KEY_VALUE, "2", KEY_END));

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 3, "expected 3 keys, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key3", "2");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key1", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY "/test/key2", "1");

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (kdb, parentKey, true);
	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);

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
	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/filter/filterkey1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	Key * recordingParentKey = keyNew ("user:/filter", KEY_END);
	elektraRecordEnableRecording (kdb, recordingParentKey, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);
	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	kdbGet (kdb, keys, parentKey);
	keyDel (ksLookupByName (keys, "user:/test/key1", KDB_O_POP));
	keyDel (ksLookupByName (keys, "user:/filter/filterkey1", KDB_O_POP));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key3", KEY_VALUE, "2", KEY_END));
	ksAppendKey (keys, keyNew ("user:/filter/filterkey2", KEY_VALUE, "2", KEY_END));

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);

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
	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);

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
	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "1", KEY_END));
	kdbSet (kdb, keys, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	elektraRecordEnableRecording (kdb, parentKey, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);
	ksClear (keys);

	// reopen KDB
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	kdbGet (kdb, keys, parentKey);
	ksClear (keys);

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Act
	bool success = elektraRecordRecord (kdb, sessionStorageKdb, keys, parentKey, errorKey);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 2, "expected 2 keys, was %zu", ksGetSize (sessionStorage));
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key1", "1");
	succeed_if_keyset_contains_key_with_string (sessionStorage, "user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key2", "1");

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (kdb, parentKey, true);
	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);

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
	KeySet * keys = ksNew (0, KS_END);
	kdbGet (kdb, keys, parentKey);
	ksAppendKey (keys, keyNew ("user:/test/key1", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:/test/key2", KEY_VALUE, "2", KEY_END));

	// We also need to store the session diff right here
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_REMOVED_KEY "/test/key3", KEY_VALUE, "3", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_MODIFIED_KEY "/test/key2", KEY_VALUE, "1", KEY_END));
	ksAppendKey (keys, keyNew ("user:" ELEKTRA_RECORD_SESSION_DIFF_ADDED_KEY "/test/key1", KEY_VALUE, "1", KEY_END));

	kdbSet (kdb, keys, parentKey);
	closePrefixedKdbInstance (kdb, parentKey, false);

	Key * errorKey = keyNew ("/", KEY_END);
	KDB * sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Act
	bool success = elektraRecordUndo (kdb, sessionStorageKdb, parentKey, errorKey);
	closePrefixedKdbInstance (kdb, parentKey, false);
	closePrefixedKdbInstance (sessionStorageKdb, parentKey, false);
	sessionStorageKdb = openPrefixedKdbInstance (contract, parentKey, __func__);

	// Assert
	succeed_if (success == true, "should return successful status key");
	succeed_if (ksGetSize (keyMeta (errorKey)) == 0, "should not have any error or warning");

	Key * sessionStorageKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);
	KeySet * sessionStorage = ksNew (0, KS_END);
	kdbGet (sessionStorageKdb, sessionStorage, sessionStorageKey);

	succeed_if_fmt (ksGetSize (sessionStorage) == 0, "expected 0 keys in session storage, was %zu", ksGetSize (sessionStorage));

	ksDel (sessionStorage);
	keyDel (sessionStorageKey);

	keyDel (parentKey);
	parentKey = keyNew ("user:/", KEY_END);

	kdb = openPrefixedKdbInstance (contract, parentKey, __func__);
	ksClear (keys);
	kdbGet (kdb, keys, parentKey);
	succeed_if_keyset_contains_key_with_string (keys, "user:/test/key3", "3");
	succeed_if_keyset_contains_key_with_string (keys, "user:/test/key2", "1");
	succeed_if (ksLookupByName (keys, "user:/test/key1", 0) == NULL, "user:/test/key1 should not have been found");

	closePrefixedKdbInstance (kdb, parentKey, false);

	ksDel (contract);
	ksDel (keys);

	closePrefixedKdbInstance (sessionStorageKdb, parentKey, true);

	keyDel (parentKey);
	keyDel (errorKey);
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

	printf ("\ntest_record RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	fflush (stdout);

	return nbError;
}
