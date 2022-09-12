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


#include <langinfo.h>

#include <tests.h>

#include "validation.h"

#include <tests_internal.h>
#include <tests_plugin.h>


#define NR_KEYS 4

void test_lookupre (void)
{
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/a", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_COMMENT, "does not match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/b", ELEKTRA_KEY_VALUE, "  a  ", ELEKTRA_KEY_COMMENT, "does not match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/c", ELEKTRA_KEY_VALUE, "\t\t", ELEKTRA_KEY_COMMENT, "match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/d", ELEKTRA_KEY_VALUE, " \t \t ", ELEKTRA_KEY_COMMENT, "match", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * match = 0;
	regex_t regex;

	regcomp (&regex, "^[ \t]*$", REG_NOSUB);

	// we start from the first key
	elektraKeysetRewind (ks);

	// show the key that match this string
	match = ksLookupRE (ks, &regex);
	succeed_if (!strcmp (elektraKeyName (match), "user:/c"), "Key did not match");

	match = ksLookupRE (ks, &regex);
	succeed_if (!strcmp (elektraKeyName (match), "user:/d"), "Key did not match");

	regfree (&regex); // free regex resources
	elektraKeysetDel (ks);
}

void test_extended (void)
{
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/a", ELEKTRA_KEY_VALUE, "la", ELEKTRA_KEY_COMMENT, "match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/b", ELEKTRA_KEY_VALUE, "lalala", ELEKTRA_KEY_COMMENT, "match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/c", ELEKTRA_KEY_VALUE, "jump", ELEKTRA_KEY_COMMENT, "does not match", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/d", ELEKTRA_KEY_VALUE, "lalalala", ELEKTRA_KEY_COMMENT, "match", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * match = 0;
	regex_t regex;

	regcomp (&regex, "^(la)+$", REG_NOSUB | REG_EXTENDED);

	// we start from the first key
	elektraKeysetRewind (ks);

	// show the key that match this string
	match = ksLookupRE (ks, &regex);
	succeed_if (!strcmp (elektraKeyName (match), "user:/a"), "Key did not match");

	match = ksLookupRE (ks, &regex);
	succeed_if (!strcmp (elektraKeyName (match), "user:/b"), "Key did not match");

	match = ksLookupRE (ks, &regex);
	succeed_if (!strcmp (elektraKeyName (match), "user:/d"), "Key did not match");

	regfree (&regex); // free regex resources
	elektraKeysetDel (ks);
}

void word_test (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/validation", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/validation/valid1", ELEKTRA_KEY_VALUE, "word", ELEKTRA_KEY_META, "check/validation", "word", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/validation/valid2", ELEKTRA_KEY_VALUE, "word1 word2 word3", ELEKTRA_KEY_META, "check/validation", "word2", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/validation/invalid1", ELEKTRA_KEY_VALUE, "aworda", ELEKTRA_KEY_META, "check/validation", "word", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/validation/invalid2", ELEKTRA_KEY_VALUE, "word1 word2 word3", ELEKTRA_KEY_META, "check/validation", "word",
			   ELEKTRA_KEY_META, "check/validation/match", "word", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks;
	PLUGIN_OPEN ("validation");

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k1);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k4);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void line_test (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/validation", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/validation/valid1", ELEKTRA_KEY_VALUE, "line", ELEKTRA_KEY_META, "check/validation", "line", ELEKTRA_KEY_META,
			   "check/validation/match", "line", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/validation/valid2", ELEKTRA_KEY_VALUE, "line1\nline2\nline3", ELEKTRA_KEY_META, "check/validation", "line2",
			   ELEKTRA_KEY_META, "check/validation/match", "line", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/validation/invalid1", ELEKTRA_KEY_VALUE, "alinea", ELEKTRA_KEY_META, "check/validation", "line", ELEKTRA_KEY_META,
			   "check/validation/match", "line", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/validation/invalid2", ELEKTRA_KEY_VALUE, "line1\nline2\nline3", ELEKTRA_KEY_META, "check/validation", "line",
			   ELEKTRA_KEY_META, "check/validation/match", "line", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks;
	PLUGIN_OPEN ("validation");

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k1);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k4);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void invert_test (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/validation", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/validation/valid1", ELEKTRA_KEY_VALUE, "word", ELEKTRA_KEY_META, "check/validation", "word", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_META, "check/validation/invert", "", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/validation/valid2", ELEKTRA_KEY_VALUE, "word1 word2 word3", ELEKTRA_KEY_META, "check/validation", "word2", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_META, "check/validation/invert", "", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/validation/invalid1", ELEKTRA_KEY_VALUE, "aworda", ELEKTRA_KEY_META, "check/validation", "word", ELEKTRA_KEY_META,
			   "check/validation/match", "word", ELEKTRA_KEY_META, "check/validation/invert", "", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/validation/invalid2", ELEKTRA_KEY_VALUE, "word1 word2 word3", ELEKTRA_KEY_META, "check/validation", "word",
			   ELEKTRA_KEY_META, "check/validation/match", "word", ELEKTRA_KEY_META, "check/validation/invert", "", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks;
	PLUGIN_OPEN ("validation");

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k1);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k4);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void icase_test (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/validation", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/validation/valid1", ELEKTRA_KEY_VALUE, "WORD", ELEKTRA_KEY_META, "check/validation", "word", ELEKTRA_KEY_META,
			   "check/validation/word", "", ELEKTRA_KEY_META, "check/validation/ignorecase", "", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/validation/valid2", ELEKTRA_KEY_VALUE, "word1 word2 word3", ELEKTRA_KEY_META, "check/validation", "wORd2", ELEKTRA_KEY_META,
			   "check/validation/word", "", ELEKTRA_KEY_META, "check/validation/ignorecase", "", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks;
	PLUGIN_OPEN ("validation");

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k1);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (2, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
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
