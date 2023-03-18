/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>

void test_readline (void)
{

	char * filename = srcdir_file ("line/linetest");
	Key * parentKey = keyNew ("user:/tests/line", KEY_VALUE, filename, KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("line");
	printf ("%s\n", filename);

	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/line/#0", 0);
	exit_if_fail (key, "line1 key not found");
	succeed_if (strcmp ("test1", keyValue (key)) == 0, "line ´ does not match");

	key = ksLookupByName (ks, "user:/tests/line/#_10", 0);
	exit_if_fail (key, "line11 key not found");
	succeed_if (strcmp ("", keyValue (key)) == 0, "line 10 should be blank");

	key = ksLookupByName (ks, "user:/tests/line/#_13", 0);
	exit_if_fail (key, "line14 key not found");
	succeed_if (strcmp ("printf(\"hello world\\n\");", keyValue (key)) == 0, "line 13 not correct");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("LINE         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_readline ();

	print_result ("test_line");

	return nbError;
}
