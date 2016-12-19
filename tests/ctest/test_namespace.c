/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_keyNamespace ()
{
	Key * key;

	printf ("Test namespaces\n");

	succeed_if (keyGetNamespace (0) == KEY_NS_NONE, "null key");

	key = keyNew (0);
	succeed_if (keyGetNamespace (key) == KEY_NS_EMPTY, "empty namespace not empty");
	succeed_if (keyNameIsSystem (keyName (key)) == 0, "empty name is not system");
	succeed_if (keyIsSystem (key) == 0, "empty key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 0, "empty name is not user");
	succeed_if (keyIsUser (key) == 0, "empty key is not user");
	keyDel (key);

	key = keyNew ("", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_EMPTY, "empty namespace not empty");
	succeed_if (keyNameIsSystem (keyName (key)) == 0, "empty name is not system");
	succeed_if (keyIsSystem (key) == 0, "empty key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 0, "empty name is not user");
	succeed_if (keyIsUser (key) == 0, "empty key is not user");
	keyDel (key);

	key = keyNew ("user", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName (key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("user/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName (key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("user:owner/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName (key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("system", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyNameIsSystem (keyName (key)) == 1, "system name is not system");
	succeed_if (keyIsSystem (key) == 1, "system key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 0, "system name is not system");
	succeed_if (keyIsUser (key) == 0, "system key is not system");
	keyDel (key);

	key = keyNew ("system/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyNameIsSystem (keyName (key)) == 1, "system name is not system");
	succeed_if (keyIsSystem (key) == 1, "system key is not system");
	succeed_if (keyNameIsUser (keyName (key)) == 0, "system name is not system");
	succeed_if (keyIsUser (key) == 0, "system key is not system");
	keyDel (key);

	key = keyNew ("spec/key", KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_SPEC, "Spec namespace not KEY_NS_SPEC");
	succeed_if (keyNameIsSpec (keyName (key)) == 1, "Spec name is not Spec");
	succeed_if (keyIsSpec (key) == 1, "Spec key is not Spec");
	succeed_if (keyNameIsUser (keyName (key)) == 0, "Spec name is not Spec");
	succeed_if (keyIsUser (key) == 0, "Spec key is not Spec");
	keyDel (key);

	key = keyNew ("/key", KEY_CASCADING_NAME, KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_CASCADING, "not correct namespace");
	keyDel (key);

	key = keyNew ("type", KEY_META_NAME, KEY_END);
	succeed_if (keyGetNamespace (key) == KEY_NS_META, "not correct namespace");
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
