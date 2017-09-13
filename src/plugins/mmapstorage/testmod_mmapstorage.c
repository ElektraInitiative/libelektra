#include <kdbprivate.h>
/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdbprivate.h>

#include <tests_plugin.h>

#include "mmapstorage.h"

//static void test_basics ()
//{
//	printf ("test basics\n");
//
//	Key * parentKey = keyNew ("user/tests/mmapstorage", KEY_END);
//	KeySet * conf = ksNew (0, KS_END);
//	PLUGIN_OPEN ("mmapstorage");
//
//	KeySet * ks = ksNew (0, KS_END);
//
//	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
//
//	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
//
//	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");
//
//	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");
//
//	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
//
//	keyDel (parentKey);
//	ksDel (ks);
//	PLUGIN_CLOSE ();
//}

static KeySet *simpleTestKeySet () {
	return ksNew (10,
		      keyNew ("user/tests/mmapstorage/simpleKey", KEY_VALUE, "root key", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/a", KEY_VALUE, "a value", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/b", KEY_VALUE, "b value", KEY_END),
		      KS_END
	);
}

static KeySet *metaTestKeySet () {
	return ksNew (10,
		      keyNew ("user/tests/mmapstorage",
			      KEY_VALUE, "root key",
			      KEY_META, "a", "b aksdjfh aksjdhf aklsjdhf aksljdhf aklsjdhf aksljdhf ",
			      KEY_END),
		      keyNew ("user/tests/mmapstorage/a",
			      KEY_VALUE, "a value",
			      KEY_META, "ab", "cd oiahsdkfhga sdjkfhgsuzdgf kashgdf aszgdf uashdf ",
			      KEY_END),
		      keyNew ("user/tests/mmapstorage/b",
			      KEY_VALUE, "b value",
			      KEY_META, "longer val", "here some even more with ugly €@\\1¹²³¼ chars",
			      KEY_END),
		      KS_END
	);
}

static void test_mmap_set_get (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	KeySet * expected = simpleTestKeySet ();
	compare_keyset(expected, returned);
	printf ("ks:\n");
	output_keyset (ks);
	printf ("expected:\n");
	output_keyset (expected);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_after_reopen (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset(expected, returned);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_empty_after_clear (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	succeed_if (ksGetSize(returned) == 0, "KeySet not empty after clear (or nullptr)");

	ksDel (returned);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmapMeta (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = metaTestKeySet();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset(expected, returned);
// 	printf ("ks:\n");
// 	output_keyset (ks);
// 	printf ("expected:\n");
// 	output_keyset (expected);
	output_keyset(ks);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmapMeta_reRead (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset(expected, ks);
// 	printf ("ks:\n");
// 	output_keyset (ks);
// 	printf ("expected:\n");
// 	output_keyset (expected);
	ksDel (expected);
	
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void clearStorage (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

#ifdef DEBUG
int cmpfunc (const void * a, const void * b)
{
   return ( *(int*)a - *(int*)b );
}

static void testDynArray1()
{
	size_t testData[] = 
{8466,2651,6624,9575,4628,9361,417,8932,4570,343,1866,3135,6617,344,9419,2094,5623,4920,2209,8037,8437,7955,5575,8355,1133,6527,8543,3338,
1772,2278,7446,8834,7728,665,8519,6079,5060,7429,3843,6923,4073,2245,2784,6620,2887,8497,9360,5752,3195,538,1491,8087,8378,5746,4961,5499,
8050,2138,1196,1860,4372,6553,4530,8828,4017,9934,3,6274,4405,5021,3416,854,4635,9902,5383,7947,5210,8242,1928,3792,7234,759,6571,9514,8451,
918,9958,1577,96,8644,6815,5584,8585,1252,808,5695,910,4157,701,77};
	
	DynArray dynArray;
	dynArray.keyArray = calloc (100, sizeof (Key *));
	dynArray.size = 0;
	dynArray.alloc = 100;
	
	for (size_t i = 0; i < 100; ++i)
	{
		findOrInsert((Key *) testData[i], &dynArray);
	}
	
	qsort (testData, 100, sizeof(size_t), cmpfunc);
	
	int error = 0;
	for (size_t i = 0; i < 100; ++i)
	{
		if (testData[i] != (size_t) dynArray.keyArray[i])
		{
			++error;
		}
	}
	
	succeed_if (error == 0, "dynArray does not sort array properly");
	
	elektraFree (dynArray.keyArray);
}
#endif

static void testMetaPreAnything ()
{
	KeySet * ks = metaTestKeySet();
	
	Key * cur;
	ksRewind(ks);
	
	while ((cur = ksNext (ks)) != 0)
	{
		if (cur->meta)
		{
			Key * curMeta;
			ksRewind(cur->meta);
			while ((curMeta = ksNext (cur->meta)) != 0)
			{
				output_key (curMeta);
			}
		}
	}
	ksDel (ks);
}



int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	
#ifdef DEBUG
	testDynArray1();
#endif
	
	//testMetaPreAnything();
	
	const char * tmpFile = elektraFilename();

	test_mmap_set_get (tmpFile);
	test_mmap_get_after_reopen (tmpFile);

	clearStorage (tmpFile);

	test_mmap_empty_after_clear (tmpFile);
	
	test_mmapMeta (tmpFile);
	test_mmapMeta_reRead (tmpFile);


	// TODO: test keyCopyMeta


	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
