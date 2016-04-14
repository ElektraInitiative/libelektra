/**
 * @file
 *
 * @brief Test suite for Libease functions accessing key name data.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdbease.h>

#include "tests.h"

#define ERROR_STRING_SIZE 1024

#define test_relative(expected, child, parent)                                                                                             \
	{                                                                                                                                  \
		char error[ERROR_STRING_SIZE];                                                                                             \
		const char * result = elektraKeyGetRelativeName (child, parent);                                                           \
		snprintf (error, sizeof (error),                                                                                           \
			  "Relative name of key was wrong.\nParent:   %s\nChild:    %s\nExpected: %s\nResult:   %s\n", keyName (parent),   \
			  keyName (child), expected, result);                                                                              \
		succeed_if (strcmp (result, expected) == 0, error);                                                                        \
	}

static void test_relative_root ()
{
	printf ("Get relative name of key with backend mounted at `/`\n");

	Key * parent = keyNew ("/", KEY_END);
	Key * child = keyNew ("spec/ni/test", KEY_END);

	test_relative ("spec/ni/test", child, parent);
	keyDel (child);
	child = keyNew ("system/💩🦄/Fjørt", KEY_END);
	test_relative ("system/💩🦄/Fjørt", child, parent);
	keyDel (child);
	child = keyNew ("user/\\/dot", KEY_END);
	test_relative ("user/\\/dot", child, parent);
	keyDel (child);
	keyDel (parent);
}

static void test_relative_cascading ()
{
	printf ("Get relative name of key with cascading mount point\n");

	Key * parent = keyNew ("/cascading", KEY_END);
	Key * child = keyNew ("/cascading/k", KEY_END);

	test_relative ("k", child, parent);
	keyDel (child);
	child = keyNew ("system/cascading/deep/deeper/deepest", KEY_END);
	test_relative ("deep/deeper/deepest", child, parent);
	keyDel (parent);
	keyDel (child);
	parent = keyNew ("/cascading\\/mountpoint/", KEY_END);
	child = keyNew ("user/cascading\\/mountpoint/\\/dot", KEY_END);
	test_relative ("\\/dot", child, parent);
	keyDel (child);
	child = keyNew ("user/second_level/cascading\\/mountpoint/\\/dot", KEY_END);
	test_relative ("\\/dot", child, parent);
	keyDel (child);
	keyDel (parent);
}

static void test_relative_generic ()
{
	printf ("Get relative name of key with generic mount point\n");

	Key * parent = keyNew ("system/", KEY_END);
	Key * child = keyNew ("system/key/🔑/🗝", KEY_END);

	test_relative ("key/🔑/🗝", child, parent);
	keyDel (child);
	child = keyNew ("system/Käfer/K", KEY_END);
	test_relative ("Käfer/K", child, parent);
	keyDel (child);
	keyDel (parent);
	parent = keyNew ("user", KEY_END);
	child = keyNew ("user/K", KEY_END);
	test_relative ("K", child, parent);
	keyDel (child);
	child = keyNew ("user/KK\\/Kitchens/What/Were/You/Thinking?", KEY_END);
	test_relative ("KK\\/Kitchens/What/Were/You/Thinking?", child, parent);
	keyDel (child);
	keyDel (parent);
}

int main (int argc, char ** argv)
{
	char * program_name = "test_keyname";
	if (argc >= 1)
	{
		program_name = argv[0];
	}

	printf ("KEY   NAME   TESTS\n");
	printf ("==================\n\n");

	test_relative_root ();
	test_relative_cascading ();
	test_relative_generic ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", program_name, nbTest, nbError);

	return nbError;
}
