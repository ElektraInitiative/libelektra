/**
 * @file
 *
 * @brief Tests for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "values.h"

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_basics ()
{
	printf ("• Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("system/elektra/modules/mini", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == KEYSET_MODIFIED, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("mINI Tests 🚙\n");
	printf ("==============\n\n");

	init (argc, argv);

	test_basics ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest != 1 ? "s" : "", nbError, nbError != 1 ? "s" : "");

	return nbError;
}
