/**
 * @file
 *
 * @brief Tests for xfconf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "xfconf.h"
#include <stdlib.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	Key * parentKey = keyNew ("user:/tests/xfconf", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	int statusCode = elektraXfconfInit (parentKey, 1, 1);
	printf ("xfconf dry open returned: %d\n", statusCode);
	if (statusCode != 1)
	{
		printf ("WARNING: dry open xfconf failed, is dbus running? skipping tests\n");
		return;
	}

	printf ("test basics\n");
	PLUGIN_OPEN ("xfconf");

	KeySet * ks = ksNew (0, KS_END);

	printf ("begin with tests...\n");

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("XFCONF     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_xfconf");

	return nbError;
}
