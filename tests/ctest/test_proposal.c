/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ksPopAtCursor (void)
{
	KeySet * ks = ksNew (5, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			     keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END), KS_END);
	KeySet * ks_c = ksNew (5, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			       keyNew ("system/valid/key1", KEY_END), KS_END);
	ksRewind (ks);
	ksNext (ks);
	ksNext (ks);
	cursor_t c = ksGetCursor (ks);
	keyDel (ksPopAtCursor (ks, c));
	succeed_if (ksCurrent (ks) == 0, "cursor position wrong");

	compare_keyset (ks, ks_c);
	ksDel (ks);
	ksDel (ks_c);
}

int main (int argc, char ** argv)
{
	printf ("KEY PROPOSAL TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksPopAtCursor ();

	printf ("\ntest_proposal RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
}
