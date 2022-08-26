/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include <kdbmerge.h>
#include <tests_internal.h>

static void test_simple(void)
{
	printf("test simple merge scenario\n");

	KeySet* ksBase = ksNew (0, KS_END);
	KeySet* ksOurs = ksNew(0, KS_END);
	KeySet* ksTheirs = ksNew (0, KS_END);

	Key* baseRoot = keyNew ("system:/test", KEY_END);
	Key* oursRoot = keyNew ("system:/test", KEY_END);
	Key* theirsRoot = keyNew ("system:/test", KEY_END);
	Key* root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));

	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	Key* information = keyNew ("system:/", KEY_END);

	KeySet* result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, 1, information);

	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 3, "result must contain 3 keys");
	succeed_if (ksLookupByName (result, "system:/test/k1", KDB_O_NONE) != NULL, "system/test/k1 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k2", KDB_O_NONE) != NULL, "system/test/k2 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k3", KDB_O_NONE) != NULL, "system/test/k3 must be included in result");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
}

int main (int argc, char ** argv)
{
	printf ("MERGE       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_simple();

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
