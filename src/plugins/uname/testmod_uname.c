/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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

void test_structure ()
{
	printf ("Test structure of keys returned from uname plugin");

	Key * parentKey = keyNew ("user/test/key", KEY_END);
	KeySet * keys = ksNew (0, KS_END);
	KeySet * conf = 0;

	PLUGIN_OPEN ("uname");

	succeed_if (plugin->kdbGet (plugin, keys, parentKey) == 1, "could not call kdbGet");

	succeed_if (ksGetSize (keys) == 6, "size not correct");
	succeed_if (ksLookupByName (keys, "user/test/key", 0), "parentkey not found");
	succeed_if (ksLookupByName (keys, "user/test/key/sysname", 0), "sysname key not found");
	succeed_if (ksLookupByName (keys, "user/test/key/nodename", 0), "nodename key not found");
	succeed_if (ksLookupByName (keys, "user/test/key/release", 0), "release key not found");
	succeed_if (ksLookupByName (keys, "user/test/key/version", 0), "version key not found");
	succeed_if (ksLookupByName (keys, "user/test/key/machine", 0), "machine key not found");

	ksDel (keys);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("UNAME       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_structure ();


	printf ("\ntestmod_uname RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
