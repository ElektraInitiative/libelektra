/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "specload.h"

#include <config.c>

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <tests_plugin.h>

#include "testdata.h"

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("spec/tests/specload", KEY_VALUE, srcdir_file ("specload/basics.quickdump"), KEY_END);
	KeySet * conf = ksNew (2, keyNew ("/app", KEY_VALUE, testapp_path, KEY_END), KS_END);

	succeed_if (elektraSpecloadCheckConfig (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "call to checkConfig was not successful");

	PLUGIN_OPEN ("specload");

	KeySet * ks = ksNew (0, KS_END);
	KeySet * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	ksDel (defaultSpec);

	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_VALUE, "0", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet did not fail");
	compare_keyset (orig, ks);

	ksDel (orig);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("SPECLOAD     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_specload");

	return nbError;
}
