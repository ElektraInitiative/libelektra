/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_cmpOrder (void)
{
	Key * k1 = keyNew ("user/a", KEY_META, "order", "20", KEY_END);
	Key * k2 = keyNew ("user/b", KEY_META, "order", "10", KEY_END);

	succeed_if (elektraKeyCmpOrder (0, 0) == 0, "null keys are not equal");
	succeed_if (elektraKeyCmpOrder (k1, 0) == 1, "not null key is not greater than null key");
	succeed_if (elektraKeyCmpOrder (0, k1) == -1, "null key is not smaller than not null key");

	succeed_if (elektraKeyCmpOrder (k1, k2) > 0, "user/a is not greater than user/b");
	succeed_if (elektraKeyCmpOrder (k2, k1) < 0, "user/b is not smaller than user/a");

	keySetMeta (k2, "order", "20");
	succeed_if (elektraKeyCmpOrder (k1, k2) == 0, "keys with same order are not equal");
	succeed_if (elektraKeyCmpOrder (k2, k1) == 0, "keys with same order are not equal");

	keySetMeta (k2, "order", 0);
	succeed_if (elektraKeyCmpOrder (k1, k2) > 0, "key with metadata is not greater than key without");
	succeed_if (elektraKeyCmpOrder (k2, k1) < 0, "key with metadata is not greater than key without");

	keySetMeta (k1, "order", 0);
	succeed_if (elektraKeyCmpOrder (k1, k2) == 0, "keys without metadata are not equal");
	succeed_if (elektraKeyCmpOrder (k2, k1) == 0, "keys without metadata are not equal");

	keyDel (k1);
	keyDel (k2);
}

static KeySet * set_a (void)
{
	return ksNew (16, keyNew ("user/0", KEY_END), keyNew ("user/a", KEY_END), keyNew ("user/a/a", KEY_END),
		      keyNew ("user/a/a/a", KEY_END), keyNew ("user/a/a/b", KEY_END), keyNew ("user/a/b", KEY_END),
		      keyNew ("user/a/b/a", KEY_END), keyNew ("user/a/b/b", KEY_END), keyNew ("user/a/c", KEY_END),
		      keyNew ("user/a/d", KEY_END), keyNew ("user/a/x/a", KEY_END), keyNew ("user/a/x/b", KEY_END),
		      keyNew ("user/a/x/c", KEY_END), keyNew ("user/a/x/c/a", KEY_END), keyNew ("user/a/x/c/b", KEY_END),
		      keyNew ("user/x", KEY_END), KS_END);
}

static void test_search (void)
{
	printf ("Testing operation search (internal)\n");

	KeySet * a = set_a ();
	Key * s = keyNew ("user/a", KEY_END);
	ssize_t result;

	keySetName (s, "user/0");
	result = ksSearchInternal (a, s);
	succeed_if (result == 0, "insertpos wrong");

	keySetName (s, "user/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 1, "insertpos wrong");

	keySetName (s, "user/a/0");
	result = ksSearchInternal (a, s);
	succeed_if (result == -3, "insertpos wrong");

	keySetName (s, "user/a/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 2, "insertpos wrong");

	keySetName (s, "user/a/a/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 3, "insertpos wrong");

	keySetName (s, "user/a/a/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 4, "insertpos wrong");

	keySetName (s, "user/a/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 5, "insertpos wrong");

	keySetName (s, "user/a/b/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 6, "insertpos wrong");

	keySetName (s, "user/a/b/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 7, "insertpos wrong");

	keySetName (s, "user/a/c");
	result = ksSearchInternal (a, s);
	succeed_if (result == 8, "insertpos wrong");

	keySetName (s, "user/a/d");
	result = ksSearchInternal (a, s);
	succeed_if (result == 9, "insertpos wrong");

	keySetName (s, "user/a/x");
	result = ksSearchInternal (a, s);
	succeed_if (result == -11, "insertpos wrong");

	keySetName (s, "user/a/x/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 10, "insertpos wrong");

	keySetName (s, "user/a/x/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 11, "insertpos wrong");

	keySetName (s, "user/a/x/c");
	result = ksSearchInternal (a, s);
	succeed_if (result == 12, "insertpos wrong");

	keySetName (s, "user/a/x/c/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 13, "insertpos wrong");

	keySetName (s, "user/a/x/c/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 14, "insertpos wrong");

	keySetName (s, "user/x");
	result = ksSearchInternal (a, s);
	succeed_if (result == 15, "insertpos wrong");

	/*
	   Generation of new test cases:
	for (int i=0; i< 16; ++i)
	{
		s = a->array[i];
		printf ("keySetName (s, \"%s\");\n", keyName(s));
		printf ("result = ksSearchInternal (a, s);\n");
		printf ("succeed_if (result == %zd, \"insertpos wrong\");\n\n", ksSearchInternal (a, s));
	}
	*/

	keyDel (s);
	ksDel (a);
}

int main (int argc, char ** argv)
{
	printf ("OPERATION    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_search ();
	test_cmpOrder ();

	printf ("\ntest_operation RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
