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
	ElektraKey * key;

	printf ("Test namespaces\n");

	succeed_if (elektraKeyGetNamespace (0) == ELEKTRA_NS_NONE, "null key");

	key = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_USER, "user namespace not KEY_NS_USER");
	elektraKeyDel (key);

	key = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_USER, "user namespace not KEY_NS_USER");
	elektraKeyDel (key);

	key = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	elektraKeyDel (key);

	key = elektraKeyNew ("system:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	elektraKeyDel (key);

	key = elektraKeyNew ("spec:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_SPEC, "Spec namespace not KEY_NS_SPEC");
	elektraKeyDel (key);

	key = elektraKeyNew ("/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_CASCADING, "not correct namespace");
	elektraKeyDel (key);

	key = elektraKeyNew ("meta:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_META, "not correct namespace");
	elektraKeyDel (key);

	key = elektraKeyNew ("proc:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_PROC, "not correct namespace");
	elektraKeyDel (key);

	key = elektraKeyNew ("dir:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_DIR, "not correct namespace");
	elektraKeyDel (key);

	key = elektraKeyNew ("default:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNamespace (key) == ELEKTRA_NS_DEFAULT, "not correct namespace");
	elektraKeyDel (key);
}

static void test_keySetNamespace (void)
{
	ElektraKey * key;

	printf ("Test set namespaces\n");

	key = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_SYSTEM) == 12, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "system:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("system:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_DIR) == 9, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "dir:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("dir:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_META) == 10, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "meta:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("meta:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_SPEC) == 10, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "spec:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("spec:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_PROC) == 10, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "proc:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_SYSTEM) == 12, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "system:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("default:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_USER) == 10, "namespace could not be set");
	succeed_if_same_string (elektraKeyName (key), "user:/key");
	elektraKeyDel (key);

	key = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetNamespace (0, ELEKTRA_NS_SYSTEM) == -1, "cannot set namespace of NULL key");
	succeed_if (elektraKeySetNamespace (key, ELEKTRA_NS_NONE) == -1, "cannot set namespace to none");
	succeed_if_same_string (elektraKeyName (key), "user:/key");
	elektraKeyDel (key);
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
