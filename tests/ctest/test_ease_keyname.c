/**
 * @file
 *
 * @brief Test suite for Libease functions accessing key name data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbease.h>

#include "tests.h"

#define ERROR_STRING_SIZE 1024

#define test_relative(expected, child, parent)                                                                                             \
	{                                                                                                                                  \
		if (child == NULL)                                                                                                         \
		{                                                                                                                          \
			yield_error ("child is null")                                                                                      \
		}                                                                                                                          \
		else if (parent == NULL)                                                                                                   \
		{                                                                                                                          \
			yield_error ("parent is null")                                                                                     \
		}                                                                                                                          \
		else                                                                                                                       \
		{                                                                                                                          \
			char error[ERROR_STRING_SIZE];                                                                                     \
			const char * result = elektraKeyGetRelativeName (child, parent);                                                   \
			snprintf (error, sizeof (error),                                                                                   \
				  "Relative name of key was wrong.\nParent:   %s\nChild:    %s\nExpected: %s\nResult:   %s\n",             \
				  keyName (parent), keyName (child), expected, result);                                                    \
			succeed_if (strcmp (result, expected) == 0, error);                                                                \
		}                                                                                                                          \
	}

static void test_relative_root (void)
{
	printf ("Get relative name of key with backend mounted at `/`\n");

	ElektraKey * parent = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * child = elektraKeyNew ("spec:/ni/test", ELEKTRA_KEY_END);

	test_relative ("spec:/ni/test", child, parent);
	elektraKeyDel (child);
	child = elektraKeyNew ("system:/💩🦄/Fjørt", ELEKTRA_KEY_END);
	test_relative ("system:/💩🦄/Fjørt", child, parent);
	elektraKeyDel (child);
	child = elektraKeyNew ("user:/\\/dot", ELEKTRA_KEY_END);
	test_relative ("user:/\\/dot", child, parent);
	elektraKeyDel (child);
	elektraKeyDel (parent);
}

static void test_relative_cascading (void)
{
	printf ("Get relative name of key with cascading mountpoint\n");

	ElektraKey * parent = elektraKeyNew ("/cascading", ELEKTRA_KEY_END);
	ElektraKey * child = elektraKeyNew ("/cascading/k", ELEKTRA_KEY_END);

	test_relative ("k", child, parent);
	elektraKeyDel (child);
	child = elektraKeyNew ("system:/cascading/deep/deeper/deepest", ELEKTRA_KEY_END);
	test_relative ("deep/deeper/deepest", child, parent);
	elektraKeyDel (parent);
	elektraKeyDel (child);
	parent = elektraKeyNew ("/cascading\\/mountpoint/", ELEKTRA_KEY_END);
	child = elektraKeyNew ("user:/cascading\\/mountpoint/\\/dot", ELEKTRA_KEY_END);
	test_relative ("\\/dot", child, parent);
	elektraKeyDel (child);
	child = elektraKeyNew ("user:/second_level/cascading\\/mountpoint/\\/dot", ELEKTRA_KEY_END);
	test_relative ("\\/dot", child, parent);
	elektraKeyDel (child);
	elektraKeyDel (parent);
}

static void test_relative_generic (void)
{
	printf ("Get relative name of key with generic mountpoint\n");

	ElektraKey * parent = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKey * child = elektraKeyNew ("system:/key//", ELEKTRA_KEY_END);

	test_relative ("key", child, parent);
	elektraKeyDel (child);
	child = elektraKeyNew ("system:/Käfer/K", ELEKTRA_KEY_END);
	test_relative ("Käfer/K", child, parent);
	elektraKeyDel (child);
	elektraKeyDel (parent);
	parent = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	child = elektraKeyNew ("user:/K", ELEKTRA_KEY_END);
	test_relative ("K", child, parent);
	test_relative ("user:/", parent, child);
	elektraKeyDel (child);
	child = elektraKeyNew ("user:/KK\\/Kitchens/What/Were/You/Thinking?", ELEKTRA_KEY_END);
	test_relative ("KK\\/Kitchens/What/Were/You/Thinking?", child, parent);
	elektraKeyDel (child);
	elektraKeyDel (parent);
}

static void test_relative_equal (void)
{
	printf ("Get relative name of key which is the same as the parent key\n");

	ElektraKey * parent = elektraKeyNew ("system:/parentChild", ELEKTRA_KEY_END);
	ElektraKey * child = elektraKeyNew ("system:/parentChild", ELEKTRA_KEY_END);
	test_relative ("", child, parent);

	elektraKeyDel (parent);
	parent = elektraKeyNew ("/parentChild", ELEKTRA_KEY_END);
	test_relative ("", child, parent);

	elektraKeyDel (child);
	elektraKeyDel (parent);
	child = elektraKeyNew ("system:/parentChild/#", ELEKTRA_KEY_END);
	parent = elektraKeyNew ("system:/parentChild/#", ELEKTRA_KEY_END);
	test_relative ("", child, parent);

	elektraKeyDel (child);
	elektraKeyDel (parent);
}

int main (int argc, char ** argv)
{
	char * program_name = "test_keyname";
	if (argc >= 1)
	{
		program_name = argv[0];
	}

	printf ("EASE KEY   NAME   TESTS\n");
	printf ("==================\n\n");

	test_relative_root ();
	test_relative_cascading ();
	test_relative_generic ();
	test_relative_equal ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", program_name, nbTest, nbError);

	return nbError;
}
