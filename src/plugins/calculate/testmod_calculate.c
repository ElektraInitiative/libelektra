/**
* @file
*
* @brief Tests for calculate plugin
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_sum()
{
	Key *parentKey = keyNew("user/tests/calculate", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/calculate/sum", KEY_VALUE, "153", KEY_META, "check/calc", "== + bla/val1 + bla/val2 bla/val3", KEY_END),
	keyNew("user/tests/calculate/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/calculate/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/calculate/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("calculate");
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}

static void test_inv_lt()
{
	Key *parentKey = keyNew("user/tests/calculate", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/calculate/sum", KEY_VALUE, "250", KEY_META, "check/calc", "< + bla/val1 + bla/val2 bla/val3", KEY_END),
	keyNew("user/tests/calculate/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/calculate/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/calculate/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("calculate");
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (-1), "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}

static void test_val_ge()
{
	Key *parentKey = keyNew("user/tests/calculate", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/calculate/sum", KEY_VALUE, "250", KEY_META, "check/calc", ">= + bla/val1 + bla/val2 bla/val3", KEY_END),
	keyNew("user/tests/calculate/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/calculate/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/calculate/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("calculate");
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (1), "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}


int main(int argc, char** argv)
{
	printf ("CALCULATE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	test_sum();
	test_inv_lt();
	test_val_ge();

	printf ("\ntestmod_calculate RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}

