/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

#define NAME_SIZE 250

static void test_ksResize (void)
{
	int i;
	KeySet * ks = 0;
	KeySet * copy = ksNew (0, KS_END);
	char name[NAME_SIZE];

	ks = ksNew (20, keyNew ("user:/test01", KEY_END), keyNew ("user:/test02", KEY_END), keyNew ("user:/test03", KEY_END),
		    keyNew ("user:/test04", KEY_END), keyNew ("user:/test05", KEY_END), keyNew ("user:/test11", KEY_END),
		    keyNew ("user:/test12", KEY_END), keyNew ("user:/test13", KEY_END), keyNew ("user:/test14", KEY_END),
		    keyNew ("user:/test15", KEY_END), keyNew ("user:/test21", KEY_END), keyNew ("user:/test22", KEY_END),
		    keyNew ("user:/test23", KEY_END), keyNew ("user:/test24", KEY_END), keyNew ("user:/test25", KEY_END),
		    keyNew ("user:/test31", KEY_END), keyNew ("user:/test32", KEY_END), keyNew ("user:/test33", KEY_END),
		    keyNew ("user:/test34", KEY_END), keyNew ("user:/test35", KEY_END), KS_END);
	succeed_if (ksGetAlloc (ks) == 20, "20 keys with alloc 20 should work");
	ksDel (ks);

	printf ("Test resize of keyset\n");
	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");
	for (i = 0; i < 100; i++)
	{
		snprintf (name, NAME_SIZE, "user:/test%d", i);
		ksAppendKey (ks, keyNew (name, KEY_END));
		if (i >= 63)
		{
			succeed_if (ksGetAlloc (ks) == 127, "allocation size wrong");
		}
		else if (i >= 31)
		{
			succeed_if (ksGetAlloc (ks) == 63, "allocation size wrong");
		}
		else if (i >= 15)
		{
			succeed_if (ksGetAlloc (ks) == 31, "allocation size wrong");
		}
		else if (i >= 0)
		{
			succeed_if (ksGetAlloc (ks) == 15, "allocation size wrong");
		}
	}
	succeed_if (ksGetSize (ks) == 100, "could not append 100 keys");
	succeed_if (ksGetAlloc (ks) == 127, "allocation size wrong");
	for (i = 100; i >= 0; i--)
	{
		keyDel (ksPop (ks));
		if (i >= 64)
		{
			succeed_if (ksGetAlloc (ks) == 127, "allocation size wrong");
		}
		else if (i >= 32)
		{
			succeed_if (ksGetAlloc (ks) == 63, "allocation size wrong");
		}
		else if (i >= 16)
		{
			succeed_if (ksGetAlloc (ks) == 31, "allocation size wrong");
		}
		else if (i >= 0)
		{
			succeed_if (ksGetAlloc (ks) == 15, "allocation size wrong");
		}
	}
	succeed_if (ksGetSize (ks) == 0, "could not pop 100 keys");
	succeed_if (ksGetAlloc (ks) == 15, "allocation size wrong");
	ksDel (ks);

	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");
	ksResize (ks, 100);
	succeed_if (ksGetAlloc (ks) == 100, "allocation size wrong");
	for (i = 0; i < 100; i++)
	{
		snprintf (name, NAME_SIZE, "user:/test%d", i);
		ksAppendKey (ks, keyNew (name, KEY_END));
		succeed_if (ksGetAlloc (ks) == 100, "allocation size wrong");
	}
	succeed_if (ksGetSize (ks) == 100, "could not append 100 keys");
	succeed_if (ksGetAlloc (ks) == 100, "allocation size wrong");
	ksDel (ks);

	ks =
#include "data_keyset.c"

		succeed_if (ksGetSize (ks) == 102, "Problem loading keyset with 102 keys");
	succeed_if (ksGetAlloc (ks) == 102, "alloc size wrong");

	ksCopy (copy, ks);
	succeed_if (ksGetSize (copy) == 102, "Problem copy keyset with 102 keys");
	// COW - copy does not allocate anything
	succeed_if (ksGetAlloc (copy) == 102, "alloc of copy size wrong");

	compare_keyset (copy, ks);

	ksClear (copy); // useless, just test for double free
	ksCopy (copy, ks);

	succeed_if (ksGetSize (copy) == 102, "Problem copy keyset with 102 keys");
	// COW - copy does not allocate anything
	succeed_if (ksGetAlloc (copy) == 102, "alloc of copy size wrong");
	compare_keyset (copy, ks);

	ksDel (copy);
	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("KEYSET SIZE  TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksResize ();

	printf ("\ntest_size RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
