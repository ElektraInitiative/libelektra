/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>

const char DE_ENV[] = "XDG_CURRENT_DESKTOP";
const char TEST_DE[] = "this_is_a_test_de";
const char NEW_DE[] = "this_is_another_test_de";

void test_desktop (void)
{

	setenv (DE_ENV, TEST_DE, 1);

	Key * parentKey = keyNew ("user:/tests/desktop", KEY_END);
	KeySet * keys = ksNew (0, KS_END);
	KeySet * conf = 0;

	PLUGIN_OPEN ("desktop")
	succeed_if (plugin->kdbGet (plugin, keys, parentKey) == 1, "could not call kdbGet")

		printf ("test if desktop key exists\n");
	succeed_if (ksGetSize (keys) == 1, "size not correct") Key const * result = ksLookupByName (keys, "user:/tests/desktop", 0);
	succeed_if (result, "desktop key not found")

		printf ("test if desktop environment is the one from the ENV\n");
	succeed_if (strcmp (keyString (result), TEST_DE) == 0, "got wrong desktop environment")

		printf ("set the desktop environment via the plugin\n");
	keySetString (parentKey, NEW_DE);
	succeed_if (plugin->kdbSet (plugin, keys, parentKey) == 0, "something changed in the keyset") result =
		ksLookupByName (keys, "user:/tests/desktop", 0);
	succeed_if (result, "desktop key not found")
		succeed_if (strcmp (keyString (result), TEST_DE) == 0, "kdb set overwrote the desktop environment")

			ksDel (keys);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("DESKTOP       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_desktop ();

	print_result ("testmod_desktop")

		return nbError;
}
