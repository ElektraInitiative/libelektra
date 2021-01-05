/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_keyNamespace (void)
{
	Key * key;

	printf ("Test namespaces\n");

	succeed_if (keyGetNamespace (0) == KEY_NS_NONE, "null key");

	key = keyNew ("user:/", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	keyDel (key);

	key = keyNew ("user:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	keyDel (key);

	key = keyNew ("system:/", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	keyDel (key);

	key = keyNew ("system:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	keyDel (key);

	key = keyNew ("spec:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SPEC, "Spec namespace not KEY_NS_SPEC");
	keyDel (key);

	key = keyNew ("/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_CASCADING, "not correct namespace");
	keyDel (key);
}

int main (int argc, char ** argv)
{
	printf ("NAMESPACE    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyNamespace ();

	printf ("\ntest_namespace RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
