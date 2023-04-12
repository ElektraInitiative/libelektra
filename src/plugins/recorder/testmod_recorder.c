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

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static kdbHookRecordPtr getRecordFunction (Plugin * plugin)
{
	return (kdbHookRecordPtr) elektraPluginGetFunction (plugin, "hook/record/record");
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

int main (int argc, char ** argv)
{
	printf ("RECORDER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_noKdbInGlobalKeySet_shouldReturnError ();

	print_result ("testmod_recorder");

	return nbError;
}
