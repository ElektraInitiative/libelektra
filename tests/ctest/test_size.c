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
	ElektraKeyset * ks = 0;
	ElektraKeyset * copy = elektraKeysetNew (0, ELEKTRA_KS_END);
	char name[NAME_SIZE];

	ks = elektraKeysetNew (20, elektraKeyNew ("user:/test01", ELEKTRA_KEY_END), elektraKeyNew ("user:/test02", ELEKTRA_KEY_END), elektraKeyNew ("user:/test03", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test04", ELEKTRA_KEY_END), elektraKeyNew ("user:/test05", ELEKTRA_KEY_END), elektraKeyNew ("user:/test11", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test12", ELEKTRA_KEY_END), elektraKeyNew ("user:/test13", ELEKTRA_KEY_END), elektraKeyNew ("user:/test14", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test15", ELEKTRA_KEY_END), elektraKeyNew ("user:/test21", ELEKTRA_KEY_END), elektraKeyNew ("user:/test22", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test23", ELEKTRA_KEY_END), elektraKeyNew ("user:/test24", ELEKTRA_KEY_END), elektraKeyNew ("user:/test25", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test31", ELEKTRA_KEY_END), elektraKeyNew ("user:/test32", ELEKTRA_KEY_END), elektraKeyNew ("user:/test33", ELEKTRA_KEY_END),
		    elektraKeyNew ("user:/test34", ELEKTRA_KEY_END), elektraKeyNew ("user:/test35", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetGetAlloc (ks) == 20, "20 keys with alloc 20 should work");
	elektraKeysetDel (ks);

	printf ("Test resize of keyset\n");
	exit_if_fail ((ks = elektraKeysetNew (0, ELEKTRA_KS_END)) != 0, "could not create new keyset");
	for (i = 0; i < 100; i++)
	{
		snprintf (name, NAME_SIZE, "user:/test%d", i);
		elektraKeysetAppendKey (ks, elektraKeyNew (name, ELEKTRA_KEY_END));
		if (i >= 63)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 127, "allocation size wrong");
		}
		else if (i >= 31)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 63, "allocation size wrong");
		}
		else if (i >= 15)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 31, "allocation size wrong");
		}
		else if (i >= 0)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 15, "allocation size wrong");
		}
	}
	succeed_if (elektraKeysetGetSize (ks) == 100, "could not append 100 keys");
	succeed_if (elektraKeysetGetAlloc (ks) == 127, "allocation size wrong");
	for (i = 100; i >= 0; i--)
	{
		elektraKeyDel (elektraKeysetPop (ks));
		if (i >= 64)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 127, "allocation size wrong");
		}
		else if (i >= 32)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 63, "allocation size wrong");
		}
		else if (i >= 16)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 31, "allocation size wrong");
		}
		else if (i >= 0)
		{
			succeed_if (elektraKeysetGetAlloc (ks) == 15, "allocation size wrong");
		}
	}
	succeed_if (elektraKeysetGetSize (ks) == 0, "could not pop 100 keys");
	succeed_if (elektraKeysetGetAlloc (ks) == 15, "allocation size wrong");
	elektraKeysetDel (ks);

	exit_if_fail ((ks = elektraKeysetNew (0, ELEKTRA_KS_END)) != 0, "could not create new keyset");
	elektraKeysetResize (ks, 100);
	succeed_if (elektraKeysetGetAlloc (ks) == 100, "allocation size wrong");
	for (i = 0; i < 100; i++)
	{
		snprintf (name, NAME_SIZE, "user:/test%d", i);
		elektraKeysetAppendKey (ks, elektraKeyNew (name, ELEKTRA_KEY_END));
		succeed_if (elektraKeysetGetAlloc (ks) == 100, "allocation size wrong");
	}
	succeed_if (elektraKeysetGetSize (ks) == 100, "could not append 100 keys");
	succeed_if (elektraKeysetGetAlloc (ks) == 100, "allocation size wrong");
	elektraKeysetDel (ks);

	ks =
#include "data_keyset.c"

		succeed_if (elektraKeysetGetSize (ks) == 102, "Problem loading keyset with 102 keys");
	succeed_if (elektraKeysetGetAlloc (ks) == 102, "alloc size wrong");

	elektraKeysetCopy (copy, ks);
	succeed_if (elektraKeysetGetSize (copy) == 102, "Problem copy keyset with 102 keys");
	succeed_if (elektraKeysetGetAlloc (copy) == 127, "alloc of copy size wrong");

	compare_keyset (copy, ks);

	elektraKeysetClear (copy); // useless, just test for double free
	elektraKeysetCopy (copy, ks);

	succeed_if (elektraKeysetGetSize (copy) == 102, "Problem copy keyset with 102 keys");
	succeed_if (elektraKeysetGetAlloc (copy) == 127, "alloc of copy size wrong");
	compare_keyset (copy, ks);

	elektraKeysetDel (copy);
	elektraKeysetDel (ks);
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
