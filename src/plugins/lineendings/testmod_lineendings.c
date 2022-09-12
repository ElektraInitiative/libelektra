/**
 * @file
 *
 * @brief Tests for lineendings plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

void testvalid (const char * file)
{
	ElektraKey * parentKey = keyNew ("user:/tests/lineendings", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = 0;
	PLUGIN_OPEN ("lineendings");
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbget failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbset failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinconsistent (const char * file)
{
	ElektraKey * parentKey = keyNew ("user:/tests/lineendings", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = 0;
	PLUGIN_OPEN ("lineendings");
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == (-1), "should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "should have failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinvalid (const char * file)
{
	ElektraKey * parentKey = keyNew ("user:/tests/lineendings", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/valid", ELEKTRA_KEY_VALUE, "CRLF", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("lineendings");
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbget failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "should have failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("LINEENDINGS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testvalid ("lineendings/valid1");
	testinconsistent ("lineendings/inconsistent");
	testinvalid ("lineendings/invalid");
	print_result ("testmod_lineendings");

	return nbError;
}
