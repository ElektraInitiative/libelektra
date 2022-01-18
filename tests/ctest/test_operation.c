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
	Key * k1 = keyNew ("user:/a", KEY_META, "order", "20", KEY_END);
	Key * k2 = keyNew ("user:/b", KEY_META, "order", "10", KEY_END);

	succeed_if (elektraKeyCmpOrder (0, 0) == 0, "null keys are not equal");
	succeed_if (elektraKeyCmpOrder (k1, 0) == 1, "not null key is not greater than null key");
	succeed_if (elektraKeyCmpOrder (0, k1) == -1, "null key is not smaller than not null key");

	succeed_if (elektraKeyCmpOrder (k1, k2) > 0, "user:/a is not greater than user:/b");
	succeed_if (elektraKeyCmpOrder (k2, k1) < 0, "user:/b is not smaller than user:/a");

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

int main (int argc, char ** argv)
{
	printf ("OPERATION    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_cmpOrder ();

	printf ("\ntest_operation RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
