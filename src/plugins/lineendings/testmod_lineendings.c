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

#include <internal/config.h>

#include <tests_plugin.h>

void testvalid (const char * file, const char * lineending)
{
	Key * parentKey = keyNew ("user:/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/valid", KEY_VALUE, lineending, KEY_END), KS_END);
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);

	/* get value should (always) be successful */
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_SUCCESS was expected)");

	/* get value should be successful */
	succeed_if (plugin->kdbCommit (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "kdbCommit returned an unexpected value (ELEKTRA_PLUGIN_STATUS_SUCCESS was expected)");

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

	/* check if no warnings are initial present in the parent key */
	succeed_if (keyGetMeta (parentKey, "warnings") == NULL, "A warning on the parentKey was present before calling kdbGet.");

	/* get value should always be successful since only warnings are produced */
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_SUCCESS was expected)");

	/* check if a warning was added to the parent key because of the previous get value */
	succeed_if (keyGetMeta (parentKey, "warnings") != NULL,
		    "A warning on the parentKey was not present after an invalid call to kdbGet.");

	/* check if no errors are initial present in the parent key */
	succeed_if (keyGetMeta (parentKey, "error") == NULL, "An error on the parentKey was present before the call the kdbCommit");

	/* set value should produce an error */
	succeed_if (plugin->kdbCommit (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_ERROR was expected)");

	/* check if an error was added to the parent key because of the previous set value */
	succeed_if (keyGetMeta (parentKey, "error") != NULL,
		    "An error on the parentKey was not present after an invalid call to kdbCommit.");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testinvalid (const char * file, const char * lineending)
{
	Key * parentKey = keyNew ("user:/tests/lineendings", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/valid", KEY_VALUE, lineending, KEY_END), KS_END);
	PLUGIN_OPEN ("lineendings");
	KeySet * ks = ksNew (0, KS_END);

	/* check if no warnings are initial present in the parent key */
	succeed_if (keyGetMeta (parentKey, "warnings") == NULL, "A warning on the parentKey was present before calling kdbGet.");

	/* get value should always be successful since only warnings are produced */
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "kdbGet returned an unexpected value (ELEKTRA_PLUGIN_STATUS_SUCCESS was expected).");

	/* check if a warning was added to the parent key because of the previous get value */
	succeed_if (keyGetMeta (parentKey, "warnings") != NULL,
		    "A warning on the parentKey was not present after an invalid call to kdbGet.");

	/* check if no errors are initial present in the parent key */
	succeed_if (keyGetMeta (parentKey, "error") == NULL, "An error on the parentKey was present before the call the kdbCommit");

	/* set value should produce an error */
	succeed_if (plugin->kdbCommit (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "kdbCommit returned an unexpected value (ELEKTRA_PLUGIN_STATUS_ERROR was expected).");

	/* check if an error was added to the parent key because of the previous set value */
	succeed_if (keyGetMeta (parentKey, "error") != NULL,
		    "An error on the parentKey was not present after an invalid call to kdbCommit.");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("LINEENDINGS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testvalid ("lineendings/valid1", "CRLF");
	testvalid ("lineendings/valid2", "LF");
	testvalid ("lineendings/valid3", "LFCR");
	testvalid ("lineendings/valid4", "CR");
	testinconsistent ("lineendings/inconsistent1");
	testinconsistent ("lineendings/inconsistent2");
	testinvalid ("lineendings/invalid1", "CRLF");
	testinvalid ("lineendings/invalid1", "LFCR");
	testinvalid ("lineendings/invalid1", "CR");
	testinvalid ("lineendings/invalid2", "LF");
	print_result ("testmod_lineendings");

	return nbError;
}
