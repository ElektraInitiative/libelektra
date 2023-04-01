#include <kdbprivate.h>
#include <kdbrecord.h>
#include <stdio.h>
#include <tests.h>

static char * appendString (const char *s1, const char *s2)
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
	succeed_if_keyset_contains_key_with_value (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	closePrefixedKdbInstance (kdb, parentKey, false);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__ );
	KeySet * tmp = ksNew (0, KS_END);
	Key * recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	succeed_if_keyset_contains_key_with_value (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	succeed_if_keyset_contains_key_with_value (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, "user:/my/test");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == true, "elektraRecordIsActive should report true");
	closePrefixedKdbInstance (kdb, parentKey, true);

	// Create new instance and disable
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__ );
	success = elektraRecordDisableRecording (kdb, recordingParentKey);
	succeed_if (success == true, "call should be successful");
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL, "global keyset should not contain active key");
	closePrefixedKdbInstance (kdb, parentKey, false);

	// Create new instance and see if it's active
	kdb = openPrefixedKdbInstance (contract, parentKey, __func__ );
	tmp = ksNew (0, KS_END);
	recordingConfigKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_END);
	kdbGet (kdb, tmp, recordingConfigKey);
	succeed_if (ksLookupByName (kdb->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL, "global keyset should not contain active key");
	succeed_if (ksLookupByName (tmp, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0) == NULL, "should not contain active key");
	ksDel (tmp);
	keyDel (recordingConfigKey);
	succeed_if (elektraRecordIsActive (kdb) == false, "elektraRecordIsActive should report false");
	closePrefixedKdbInstance (kdb, parentKey, true);

	ksDel (contract);
	keyDel (parentKey);
	keyDel (recordingParentKey);
}





int main (int argc, char ** argv)
{
	printf ("RECORD       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraRecordIsActive_onEmptyKdb ();
	test_elektraRecordEnableRecording_isActive_Disable_shouldWork ();

	printf ("\ntest_record RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
