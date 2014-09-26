#include <kdbproposal.h>

#include "tests.h"

static void test_array()
{
	printf ("Test array\n");

	Key *k = keyNew("user/array/#0", KEY_END);
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#1");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#2");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#3");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#4");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#5");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#6");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#7");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#8");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#9");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_10");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_11");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_12");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_13");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_14");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_15");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_16");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_17");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_18");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_19");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_20");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_21");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_22");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_23");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_24");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_25");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_26");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_27");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_28");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_29");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	for (int i = 30; i<99; ++i)
	{
		succeed_if(!elektraArrayIncName(k), "increment array entry in loop returned error");
	}
	succeed_if_same_string (keyName(k), "user/array/#_99");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#__100");
	for (int i = 101; i<1000; ++i)
	{
		succeed_if(!elektraArrayIncName(k), "increment array entry in loop returned error");
	}
	succeed_if_same_string (keyName(k), "user/array/#__999");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#___1000");
	keySetBaseName(k, "#_________4000000000");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if_same_string (keyName(k), "user/array/#_________4000000001");
	keyDel(k);
}

static void test_noArray()
{
	printf ("Test no array\n");
	Key *k = keyNew("user/noarray", KEY_END);

	succeed_if(elektraArrayIncName(0) == -1, "null pointer");
	succeed_if(elektraArrayIncName(k) == -1, "no array");

	keyDel(k);
}

static void test_startArray()
{
	printf ("Test start array\n");
	Key *k = keyNew("user/startarray/#", KEY_END);

	succeed_if(elektraArrayIncName(k) == 0, "no array start");
	succeed_if_same_string (keyName(k), "user/startarray/#0");
	succeed_if(elektraArrayIncName(k) == 0, "no array inc");
	succeed_if_same_string (keyName(k), "user/startarray/#1");

	keyDel(k);
}


int main(int argc, char** argv)
{
	printf(" ARRAY   TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_array();
	test_noArray();
	test_startArray();

	printf("\ntest_array RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
