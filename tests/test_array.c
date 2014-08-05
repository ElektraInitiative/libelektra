#include <kdbproposal.h>

#include "tests.h"

void test_array()
{
	printf ("Test array\n");

	Key *k = keyNew("user/array", KEY_END);
	succeed_if(!elektraArrayIncName(k), "increment init array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#0"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#1"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#2"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#3"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#4"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#5"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#6"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#7"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#8"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#9"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_10"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_11"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_12"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_13"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_14"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_15"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_16"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_17"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_18"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_19"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_20"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_21"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_22"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_23"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_24"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_25"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_26"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_27"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_28"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_29"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	for (int i = 30; i<99; ++i)
	{
		succeed_if(!elektraArrayIncName(k), "increment array entry in loop returned error");
	}
	succeed_if(!strcmp(keyName(k), "user/array/#_99"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#__100"), "array entry name not correct");
	for (int i = 101; i<1000; ++i)
	{
		succeed_if(!elektraArrayIncName(k), "increment array entry in loop returned error");
	}
	succeed_if(!strcmp(keyName(k), "user/array/#__999"), "array entry name not correct");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#___1000"), "array entry name not correct");
	keySetBaseName(k, "#_________4000000000");
	succeed_if(!elektraArrayIncName(k), "increment array entry name returned error");
	succeed_if(!strcmp(keyName(k), "user/array/#_________4000000001"), "array entry name not correct");
	keyDel(k);
}

int main(int argc, char** argv)
{
	printf(" ARRAY   TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_array();

	printf("\ntest_array RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
