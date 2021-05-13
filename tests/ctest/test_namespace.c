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

	key = keyNew ("meta:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_META, "not correct namespace");
	keyDel (key);

	key = keyNew ("proc:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_PROC, "not correct namespace");
	keyDel (key);

	key = keyNew ("dir:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_DIR, "not correct namespace");
	keyDel (key);

	key = keyNew ("default:/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_DEFAULT, "not correct namespace");
	keyDel (key);
}

static void test_keySetNamespace (void)
{
	Key * key;

	printf ("Test set namespaces\n");

	key = keyNew ("user:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_SYSTEM) == 12, "namespace could not be set");
	succeed_if_same_string (keyName (key), "system:/key");
	keyDel (key);

	key = keyNew ("system:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_DIR) == 9, "namespace could not be set");
	succeed_if_same_string (keyName (key), "dir:/key");
	keyDel (key);

	key = keyNew ("dir:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_META) == 10, "namespace could not be set");
	succeed_if_same_string (keyName (key), "meta:/key");
	keyDel (key);

	key = keyNew ("meta:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_SPEC) == 10, "namespace could not be set");
	succeed_if_same_string (keyName (key), "spec:/key");
	keyDel (key);

	key = keyNew ("spec:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_PROC) == 10, "namespace could not be set");
	succeed_if_same_string (keyName (key), "proc:/key");
	keyDel (key);

	key = keyNew ("/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_SYSTEM) == 12, "namespace could not be set");
	succeed_if_same_string (keyName (key), "system:/key");
	keyDel (key);

	key = keyNew ("default:/key", KEY_END);
	succeed_if (keySetNamespace (key, KEY_NS_USER) == 10, "namespace could not be set");
	succeed_if_same_string (keyName (key), "user:/key");
	keyDel (key);

	key = keyNew ("user:/key", KEY_END);
	succeed_if (keySetNamespace (0, KEY_NS_SYSTEM) == -1, "cannot set namespace of NULL key");
	succeed_if (keySetNamespace (key, KEY_NS_NONE) == -1, "cannot set namespace to none");
	succeed_if_same_string (keyName (key), "user:/key");
	keyDel (key);
}

int main (int argc, char ** argv)
{
	printf ("NAMESPACE    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyNamespace ();
	test_keySetNamespace ();

	printf ("\ntest_namespace RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
