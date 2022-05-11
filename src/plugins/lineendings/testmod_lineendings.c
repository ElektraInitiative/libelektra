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
	Key * parentKey = keyNew ("user:/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_NO_UPDATE was expected)");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "kdbSet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_SUCCESS was expected)");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinconsistent (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);


	succeed_if (keyGetMeta (parentKey, "warnings") == NULL, "A warning on the parentKey was present before calling kdbGet.");

	/* get value */
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_NO_UPDATE was expected)");

	/* check if a warning was added to the parent key */
	succeed_if (keyGetMeta (parentKey, "warnings") != NULL,
		    "A warning on the parentKey was not present after an invalid call to kdbGet.");

	succeed_if (keyGetMeta (parentKey, "error") == NULL, "An error on the parentKey was present before the call the kdbSet");

	/* set value */
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_ERROR was expected)");

	succeed_if (keyGetMeta (parentKey, "error") != NULL, "An error on the parentKey was not present after an invalid call to kdbSet.");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinvalid (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/valid", KEY_VALUE, "CRLF", KEY_END), KS_END);
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (keyGetMeta (parentKey, "warnings") == NULL, "A warning on the parentKey was present before calling kdbGet.");

	/* get value */
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_NO_UPDATE was expected).");

	/* check if a warning was added to the parent key */
	succeed_if (keyGetMeta (parentKey, "warnings") != NULL,
		    "A warning on the parentKey was not present after an invalid call to kdbGet.");


	succeed_if (keyGetMeta (parentKey, "error") == NULL, "An error on the parentKey was present before the call the kdbSet");

	/* set value */
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "kdbSet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_ERROR was expected).");

	succeed_if (keyGetMeta (parentKey, "error") != NULL, "An error on the parentKey was not present after an invalid call to kdbSet.");
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
