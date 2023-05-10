/**
 * @file
 *
 * @brief Tests for recorder plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbprivate.h>
#include <stdlib.h>
#include <tests_plugin.h>
#include <unistd.h>

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static kdbHookRecordPtr getRecordFunction (Plugin * plugin)
{
	return (kdbHookRecordPtr) elektraPluginGetFunction (plugin, "hook/record/record");
}

static kdbHookRecordLockPtr getLockFunction (Plugin * plugin)
{
	return (kdbHookRecordLockPtr) elektraPluginGetFunction (plugin, "hook/record/lock");
}

static kdbHookRecordUnlockPtr getUnlockFunction (Plugin * plugin)
{
	return (kdbHookRecordUnlockPtr) elektraPluginGetFunction (plugin, "hook/record/unlock");
}

static void test_noKdbInGlobalKeySet_shouldReturnError (void)
{
	printf ("Test %s\n", __func__);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * returned = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	succeed_if (getRecordFunction (plugin) (plugin, returned, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call should return error");

	ksDel (returned);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_locking_should_work (void)
{
	printf ("Test %s\n", __func__);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * returned = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	plugin->global = ksNew (1, keyNew ("system:/elektra/kdb", KEY_BINARY, KEY_SIZE, sizeof (kdb), KEY_VALUE, &kdb, KEY_END), KS_END);
	ksAppendKey (kdb->global, keyNew ("system:" ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_VALUE, keyName (parentKey), KEY_END));


	int lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_SUCCESS, "first lock should be successful");

	lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_ERROR, "second lock should not be successful");

	getUnlockFunction (plugin) (plugin, parentKey);

	lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_SUCCESS, "third lock should be successful");

	getUnlockFunction (plugin) (plugin, parentKey);


	ksDel (plugin->global);
	PLUGIN_CLOSE ();
	kdbClose (kdb, parentKey);
	ksDel (returned);
	keyDel (parentKey);
}

static void test_locking_without_recording_should_always_work (void)
{
	printf ("Test %s\n", __func__);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * returned = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	plugin->global = ksNew (1, keyNew ("system:/elektra/kdb", KEY_BINARY, KEY_SIZE, sizeof (kdb), KEY_VALUE, &kdb, KEY_END), KS_END);

	int lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_SUCCESS, "first lock should be successful");

	lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_SUCCESS, "second lock should be successful");

	ksDel (plugin->global);
	PLUGIN_CLOSE ();
	kdbClose (kdb, parentKey);
	ksDel (returned);
	keyDel (parentKey);
}

static void test_locking_should_create_specified_lockfile (void)
{
	printf ("Test %s\n", __func__);

	const char * lockfileName = "/tmp/my_test_elektra_record_lockfile";

	KeySet * conf = ksNew (1, keyNew ("/lockfile", KEY_VALUE, lockfileName, KEY_END), KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * returned = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	plugin->global = ksNew (1, keyNew ("system:/elektra/kdb", KEY_BINARY, KEY_SIZE, sizeof (kdb), KEY_VALUE, &kdb, KEY_END), KS_END);
	ksAppendKey (kdb->global, keyNew ("system:" ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_VALUE, keyName (parentKey), KEY_END));


	int lockResult = getLockFunction (plugin) (plugin, parentKey);
	succeed_if (lockResult == ELEKTRA_PLUGIN_STATUS_SUCCESS, "first lock should be successful");

	int accessResult = access (lockfileName, F_OK);
	succeed_if_fmt (accessResult == 0, "file %s should exist", lockfileName);

	getUnlockFunction (plugin) (plugin, parentKey);

	ksDel (plugin->global);
	PLUGIN_CLOSE ();
	kdbClose (kdb, parentKey);
	ksDel (returned);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("RECORDER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_noKdbInGlobalKeySet_shouldReturnError ();
	test_locking_should_create_specified_lockfile ();
	test_locking_without_recording_should_always_work ();
	test_locking_should_work ();

	print_result ("testmod_recorder");

	return nbError;
}
