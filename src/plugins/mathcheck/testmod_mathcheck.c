/**
 * @file
 *
 * @brief Tests for mathcheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./floathelper.h"
#include <internal/kdb/config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

#define test(ks, retval)                                                                                                                   \
	{                                                                                                                                  \
		Key * parentKey = keyNew ("user:/tests/mathcheck", KEY_VALUE, "", KEY_END);                                                \
		KeySet * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("mathcheck");                                                                                                 \
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == retval, "error");                                                    \
		keyDel (parentKey);                                                                                                        \
		PLUGIN_CLOSE ();                                                                                                           \
	}

#define testSet(ks, value)                                                                                                                 \
	{                                                                                                                                  \
		Key * parentKey = keyNew ("user:/tests/mathcheck", KEY_VALUE, "", KEY_END);                                                \
		KeySet * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("mathcheck");                                                                                                 \
		plugin->kdbSet (plugin, ks, parentKey);                                                                                    \
		succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/mathcheck/sum", 0)), value), "error");                    \
		keyDel (parentKey);                                                                                                        \
		PLUGIN_CLOSE ();                                                                                                           \
	}

static KeySet * create_ks (const char * res, const char * meta)
{
	return ksNew (5, keyNew ("user:/tests/mathcheck/sum", KEY_VALUE, res, KEY_META, "check/math", meta, KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val1", KEY_VALUE, "100", KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val2", KEY_VALUE, "50", KEY_END),
		      keyNew ("user:/tests/mathcheck/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
}

static void test_multiUp (void)
{
	Key * parentKey = keyNew ("user:/tests/mathcheck", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/mathcheck/up/sum", KEY_VALUE, "0", KEY_META, "check/math",
				     ":= + ../val1 + ../../val2 ../val3", KEY_END),
			     keyNew ("user:/tests/mathcheck/up/val1", KEY_VALUE, "1", KEY_END),
			     keyNew ("user:/tests/mathcheck/val2", KEY_VALUE, "2", KEY_END),
			     keyNew ("user:/tests/mathcheck/up/val3", KEY_VALUE, "10", KEY_END), KS_END);

	PLUGIN_OPEN ("mathcheck");
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

	KeySet * ks = create_ks ("153", "== + ../bla/val1 + ../bla/val2 ../bla/val3");
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
