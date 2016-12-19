/**
 * @file
 *
 * @brief Tests for lineendings plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

void testvalid (const char * file)
{
	Key * parentKey = keyNew ("user/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbget failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbset failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinconsistent (const char * file)
{
	Key * parentKey = keyNew ("user/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == (-1), "should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "should have failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinvalid (const char * file)
{
	Key * parentKey = keyNew ("user/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system/valid", KEY_VALUE, "CRLF", KEY_END), KS_END);
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);
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
	printf ("\ntestmod_lineendings RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
