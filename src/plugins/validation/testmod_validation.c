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


#include <langinfo.h>

#include <tests.h>

#include "./validation.h"

#include <tests_internal.h>
#include <tests_plugin.h>


#define NR_KEYS 4

void test_lookupre (void)
{
	KeySet * ks = ksNew (5, keyNew ("user:/a", KEY_VALUE, "a", KEY_META, "comment/#0", "does not match", KEY_END),
			     keyNew ("user:/b", KEY_VALUE, "  a  ", KEY_META, "comment/#0", "does not match", KEY_END),
			     keyNew ("user:/c", KEY_VALUE, "\t\t", KEY_META, "comment/#0", "match", KEY_END),
			     keyNew ("user:/d", KEY_VALUE, " \t \t ", KEY_META, "comment/#0", "match", KEY_END), KS_END);

	Key * match = 0;
	regex_t regex;

	regcomp (&regex, "^[ \t]*$", REG_NOSUB);


	// show the key that match this string
	match = ksLookupRE (ks, &regex, 0);
	succeed_if (!strcmp (keyName (match), "user:/c"), "Key did not match");

	match = ksLookupRE (ks, &regex, ksSearch (ks, match) + 1);
	succeed_if (!strcmp (keyName (match), "user:/d"), "Key did not match");

	regfree (&regex); // free regex resources
	ksDel (ks);
}

void test_extended (void)
{
	KeySet * ks = ksNew (5, keyNew ("user:/a", KEY_VALUE, "la", KEY_META, "comment/#0", "match", KEY_END),
			     keyNew ("user:/b", KEY_VALUE, "lalala", KEY_META, "comment/#0", "match", KEY_END),
			     keyNew ("user:/c", KEY_VALUE, "jump", KEY_META, "comment/#0", "does not match", KEY_END),
			     keyNew ("user:/d", KEY_VALUE, "lalalala", KEY_META, "comment/#0", "match", KEY_END), KS_END);

	Key * match = 0;
	regex_t regex;

	regcomp (&regex, "^(la)+$", REG_NOSUB | REG_EXTENDED);


	// show the key that match this string
	match = ksLookupRE (ks, &regex, 0);
	succeed_if (!strcmp (keyName (match), "user:/a"), "Key did not match");

	match = ksLookupRE (ks, &regex, ksSearch (ks, match) + 1);
	succeed_if (!strcmp (keyName (match), "user:/b"), "Key did not match");

	match = ksLookupRE (ks, &regex, ksSearch (ks, match) + 1);
	succeed_if (!strcmp (keyName (match), "user:/d"), "Key did not match");

	regfree (&regex); // free regex resources
	ksDel (ks);
}

void word_test (void)
{
	Key * parentKey = keyNew ("user:/tests/validation", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/validation/valid1", KEY_VALUE, "word", KEY_META, "check/validation", "word", KEY_META,
			   "check/validation/match", "word", KEY_END);
	Key * k2 = keyNew ("user:/tests/validation/valid2", KEY_VALUE, "word1 word2 word3", KEY_META, "check/validation", "word2", KEY_META,
			   "check/validation/match", "word", KEY_END);
	Key * k3 = keyNew ("user:/tests/validation/invalid1", KEY_VALUE, "aworda", KEY_META, "check/validation", "word", KEY_META,
			   "check/validation/match", "word", KEY_END);
	Key * k4 = keyNew ("user:/tests/validation/invalid2", KEY_VALUE, "word1 word2 word3", KEY_META, "check/validation", "word",
			   KEY_META, "check/validation/match", "word", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("validation");

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k1);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k4);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void line_test (void)
{
	Key * parentKey = keyNew ("user:/tests/validation", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/validation/valid1", KEY_VALUE, "line", KEY_META, "check/validation", "line", KEY_META,
			   "check/validation/match", "line", KEY_END);
	Key * k2 = keyNew ("user:/tests/validation/valid2", KEY_VALUE, "line1\nline2\nline3", KEY_META, "check/validation", "line2",
			   KEY_META, "check/validation/match", "line", KEY_END);
	Key * k3 = keyNew ("user:/tests/validation/invalid1", KEY_VALUE, "alinea", KEY_META, "check/validation", "line", KEY_META,
			   "check/validation/match", "line", KEY_END);
	Key * k4 = keyNew ("user:/tests/validation/invalid2", KEY_VALUE, "line1\nline2\nline3", KEY_META, "check/validation", "line",
			   KEY_META, "check/validation/match", "line", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("validation");

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k1);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k4);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void invert_test (void)
{
	Key * parentKey = keyNew ("user:/tests/validation", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/validation/valid1", KEY_VALUE, "word", KEY_META, "check/validation", "word", KEY_META,
			   "check/validation/match", "word", KEY_META, "check/validation/invert", "", KEY_END);
	Key * k2 = keyNew ("user:/tests/validation/valid2", KEY_VALUE, "word1 word2 word3", KEY_META, "check/validation", "word2", KEY_META,
			   "check/validation/match", "word", KEY_META, "check/validation/invert", "", KEY_END);
	Key * k3 = keyNew ("user:/tests/validation/invalid1", KEY_VALUE, "aworda", KEY_META, "check/validation", "word", KEY_META,
			   "check/validation/match", "word", KEY_META, "check/validation/invert", "", KEY_END);
	Key * k4 = keyNew ("user:/tests/validation/invalid2", KEY_VALUE, "word1 word2 word3", KEY_META, "check/validation", "word",
			   KEY_META, "check/validation/match", "word", KEY_META, "check/validation/invert", "", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("validation");

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k1);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k4);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void icase_test (void)
{
	Key * parentKey = keyNew ("user:/tests/validation", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/validation/valid1", KEY_VALUE, "WORD", KEY_META, "check/validation", "word", KEY_META,
			   "check/validation/word", "", KEY_META, "check/validation/ignorecase", "", KEY_END);
	Key * k2 = keyNew ("user:/tests/validation/valid2", KEY_VALUE, "word1 word2 word3", KEY_META, "check/validation", "wORd2", KEY_META,
			   "check/validation/word", "", KEY_META, "check/validation/ignorecase", "", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("validation");

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k1);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (2, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("   VALIDATION   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_lookupre ();
	test_extended ();

	word_test ();
	line_test ();
	icase_test ();
	invert_test ();
	print_result ("testmod_validation");

	return nbError;
}
