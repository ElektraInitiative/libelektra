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

void test_readline (void)
{

	char * filename = srcdir_file ("line/linetest");
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/line", ELEKTRA_KEY_VALUE, filename, ELEKTRA_KEY_END);
	ElektraKeyset * conf = 0;
	PLUGIN_OPEN ("line");
	printf ("%s\n", filename);

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/line/#0", 0);
	exit_if_fail (key, "line1 key not found");
	succeed_if (strcmp ("test1", elektraKeyValue (key)) == 0, "line ´ does not match");

	key = elektraKeysetLookupByName (ks, "user:/tests/line/#_10", 0);
	exit_if_fail (key, "line11 key not found");
	succeed_if (strcmp ("", elektraKeyValue (key)) == 0, "line 10 should be blank");

	key = elektraKeysetLookupByName (ks, "user:/tests/line/#_13", 0);
	exit_if_fail (key, "line14 key not found");
	succeed_if (strcmp ("printf(\"hello world\\n\");", elektraKeyValue (key)) == 0, "line 13 not correct");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

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
