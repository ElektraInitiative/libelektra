/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "tests.h"
#include <elektra/ease/array.h>

static void test_array (void)
{
	printf ("Test array\n");

	Key * k = keyNew ("user:/array/#0", KEY_END);
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#1");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#2");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#3");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#4");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#5");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#6");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#7");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#8");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#9");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_10");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_11");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_12");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_13");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_14");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_15");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_16");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_17");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_18");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_19");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_20");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_21");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_22");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_23");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_24");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_25");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_26");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_27");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_28");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_29");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	for (int i = 30; i < 99; ++i)
	{
		succeed_if (!elektraArrayIncName (k), "increment array entry in loop returned error");
	}
	succeed_if_same_string (keyName (k), "user:/array/#_99");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#__100");
	for (int i = 101; i < 1000; ++i)
	{
		succeed_if (!elektraArrayIncName (k), "increment array entry in loop returned error");
	}
	succeed_if_same_string (keyName (k), "user:/array/#__999");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#___1000");
	keySetBaseName (k, "#_________4000000000");
	succeed_if (!elektraArrayIncName (k), "increment array entry name returned error");
	succeed_if_same_string (keyName (k), "user:/array/#_________4000000001");
	keyDel (k);
}

static void test_arrayDec (void)
{
	printf ("Decrement array indizes\n");

	Key * k = keyNew ("user:/array/#0", KEY_END);
	succeed_if (elektraArrayDecName (k) == -1, "Decrementing array index 0 did not fail");
	keyDel (k);

	k = keyNew ("user:/array/#___1337", KEY_END);
	succeed_if (elektraArrayDecName (k) == 0, "Unable to decrement array index 1337");
	succeed_if_same_string (keyName (k), "user:/array/#___1336");
	succeed_if (elektraArrayDecName (k) == 0, "Unable to decrement array index 1336");
	succeed_if_same_string (keyName (k), "user:/array/#___1335");
	keyDel (k);

	k = keyNew ("user:/array/#_________4000000000", KEY_END);
	succeed_if (elektraArrayDecName (k) == 0, "Unable to decrement array index 4000000000");
	succeed_if_same_string (keyName (k), "user:/array/#_________3999999999");
	keyDel (k);
}

static void test_noArray (void)
{
	printf ("Test no array\n");
	Key * k = keyNew ("user:/noarray", KEY_END);

	succeed_if (elektraArrayIncName (0) == -1, "null pointer");
	succeed_if (elektraArrayIncName (k) == -1, "no array");

	keyDel (k);
}

static void test_startArray (void)
{
	printf ("Test start array\n");
	Key * k = keyNew ("user:/startarray/#", KEY_END);

	succeed_if (elektraArrayIncName (k) == 0, "no array start");
	succeed_if_same_string (keyName (k), "user:/startarray/#0");
	succeed_if (elektraArrayIncName (k) == 0, "no array inc");
	succeed_if_same_string (keyName (k), "user:/startarray/#1");

	keyDel (k);
}

static void test_getArray (void)
{
	printf ("Test get array");

	KeySet * keys =
		ksNew (10, keyNew ("user:/test/key1", KEY_END), keyNew ("user:/test/key2", KEY_END), keyNew ("user:/test/array", KEY_END),
		       keyNew ("user:/test/array/#0", KEY_END), keyNew ("user:/test/array/#0/below", KEY_END),
		       keyNew ("user:/test/array/#1", KEY_END), keyNew ("user:/test/yetanotherkey", KEY_END), KS_END);

	Key * arrayParent = keyNew ("user:/test/array", KEY_END);
	KeySet * array = elektraArrayGet (arrayParent, keys);

	succeed_if (array, "The getarray function did not return a proper keyset");
	succeed_if (ksGetSize (array) == 2, "the array contains a wrong number of elements");
	succeed_if (ksLookupByName (array, "user:/test/array/#0", KDB_O_NONE), "the array does not contain #0");
	succeed_if (ksLookupByName (array, "user:/test/array/#1", KDB_O_NONE), "the array does not contain #1");

	keyDel (arrayParent);
	ksDel (array);
	ksDel (keys);
}


static void test_getArrayNext (void)
{
	printf ("Test get array next");

	KeySet * array = ksNew (10, keyNew ("user:/test/array/#0", KEY_END), keyNew ("user:/test/array/#1", KEY_END),

				KS_END);

	Key * nextKey = elektraArrayGetNextKey (array);
	exit_if_fail (array, "The getnext function did not return a proper key");
	succeed_if (!strcmp (keyName (nextKey), "user:/test/array/#2"), "The getnext function did not use the correct keyname");
	succeed_if (!strcmp (keyString (nextKey), ""), "The getnext function did not return an empty key");

	keyDel (nextKey);
	ksClear (array);
	nextKey = elektraArrayGetNextKey (array);
	succeed_if (!nextKey, "The getnext function did not return NULL on an empty array");
	keyDel (nextKey);

	ksDel (array);
}

static void test_baseName (void)
{
	printf ("Test validate base name");

	succeed_if (elektraArrayValidateBaseNameString ("#") == 0, "Start not detected correctly");
	succeed_if (elektraArrayValidateBaseNameString ("#0") == 1, "#0 should be valid");
	succeed_if (elektraArrayValidateBaseNameString ("#_10") == 2, "#_10 should be valid");
	succeed_if (elektraArrayValidateBaseNameString ("#_________1234567890") == 10, "#_________1234567890 should be valid");
	succeed_if (elektraArrayValidateBaseNameString ("#___________________12345678901234567890") == 20,
		    "#_________1234567890 should be valid");
	succeed_if (elektraArrayValidateBaseNameString ("#___________________123456789012345678901") == -1,
		    "#__________12345678901 should not be valid");
	succeed_if (elektraArrayValidateBaseNameString ("monkey") == -1, "monkey should not be valid");
}

int main (int argc, char ** argv)
{
	printf (" ARRAY   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_array ();
	test_arrayDec ();
	test_noArray ();
	test_startArray ();
	test_getArray ();
	test_getArrayNext ();
	test_baseName ();

	printf ("\ntest_array RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
