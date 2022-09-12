/**
 * @file
 *
 * @brief Tests for mathcheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "floathelper.h"
#include <kdbconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

#define test(ks, retval)                                                                                                                   \
	{                                                                                                                                  \
		ElektraKey * parentKey = keyNew ("user:/tests/mathcheck", KEY_VALUE, "", KEY_END);                                                \
		ElektraKeyset * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("mathcheck");                                                                                                 \
		ksRewind (ks);                                                                                                             \
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == retval, "error");                                                    \
		keyDel (parentKey);                                                                                                        \
		PLUGIN_CLOSE ();                                                                                                           \
	}

#define testSet(ks, value)                                                                                                                 \
	{                                                                                                                                  \
		ElektraKey * parentKey = keyNew ("user:/tests/mathcheck", KEY_VALUE, "", KEY_END);                                                \
		ElektraKeyset * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("mathcheck");                                                                                                 \
		ksRewind (ks);                                                                                                             \
		plugin->kdbSet (plugin, ks, parentKey);                                                                                    \
		succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/mathcheck/sum", 0)), value), "error");                    \
		keyDel (parentKey);                                                                                                        \
		PLUGIN_CLOSE ();                                                                                                           \
	}

static ElektraKeyset * create_ks (const char * res, const char * meta)
{
	return ksNew (5, keyNew ("user:/tests/mathcheck/sum", ELEKTRA_KEY_VALUE, res, ELEKTRA_KEY_META, "check/math", meta, ELEKTRA_KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_multiUp (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/mathcheck", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = ksNew (5,
			     keyNew ("user:/tests/mathcheck/up/sum", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/math",
				     ":= + ../val1 + ../../val2 ../val3", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/mathcheck/up/val1", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/mathcheck/val2", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/mathcheck/up/val3", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	PLUGIN_OPEN ("mathcheck");
	ksRewind (ks);
	plugin->kdbSet (plugin, ks, parentKey);
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/mathcheck/up/sum", 0)), "13"), "error");
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("MATHCHECK	   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	ElektraKeyset * ks = create_ks ("153", "== + ../bla/val1 + ../bla/val2 ../bla/val3");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("250", "< + ../bla/val1 + ../bla/val2 ../bla/val3");
	test (ks, (-1));
	ksDel (ks);

	ks = create_ks ("250", ">= + @/bla/val1 + @/bla/val2 @/bla/val3");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("2", "== / @/bla/val1 @/bla/val2");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("", ":= / @/bla/val1 @/bla/val2");
	testSet (ks, "2");
	ksDel (ks);

	ks = create_ks ("1", "== / ../bla/val1 ../bla/val3");
	test (ks, (-1));
	ksDel (ks);

	ks = create_ks ("3", "== + '1.5' '1.5'");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("4.5", "== + '1.5' + '1.5' '1.5'");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("", ":= + '1.5' + '1.5' '1.5'");
	testSet (ks, "4.5");
	ksDel (ks);

	ks = create_ks ("1", "== + '1.5' '1.5'");
	test (ks, (-1));
	ksDel (ks);

	ks = create_ks ("10", "== + ../bla/val3 '7'");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("7", "== + @/bla/nonExisting '7'");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("", ":= + @/bla/nonExisting '7'");
	testSet (ks, "7");
	ksDel (ks);

	ks = create_ks ("7", "== * @/bla/nonExisting '7'");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("3", "== + ../bla/nonExisting + ../bla/nonExistingToo ../bla/val3");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("", ":= + ../bla/nonExisting + ../bla/nonExistingToo ../bla/val3");
	testSet (ks, "3");
	ksDel (ks);

	ks = create_ks ("3", "== / @/bla/nonExisting / ../bla/nonExistingToo @/bla/val3");
	test (ks, 1);
	ksDel (ks);

	ks = create_ks ("3", "== + @/bla/nonExisting / ../bla/val3 ../bla/nonExistingToo");
	test (ks, 1);
	ksDel (ks);

	test_multiUp ();

	print_result ("testmod_mathcheck");

	char buffer[24];
	elektraFtoA (buffer, sizeof (buffer), (1.5));
	succeed_if (!(strcmp (buffer, "1.5")), "elektraFtoA failed");
	fprintf (stderr, "elektraFtoA: val: %g, ret: %s\n", (1.5), buffer);
	fprintf (stderr, "elektraEFtoF: string: %s, ret: %g\n", buffer, elektraEFtoF (buffer));
	succeed_if ((elektraEFtoF (buffer) - (1.5)) < 0.00001, "elektraEFtoF failed");
	return nbError;
}
