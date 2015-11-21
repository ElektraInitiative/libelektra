/**
* @file
*
* @brief Tests for enum plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <stdio.h>

#include <tests_plugin.h>
#include <tests_internal.h>


static void test()
{
	Key *parentKey = keyNew("user/tests/enum", KEY_VALUE, "", KEY_END);
	Key *k1 = keyNew("user/tests/enum/valid1", KEY_VALUE, "TRUE", KEY_META, "check/enum", "'TRUE','FALSE'", KEY_END);
	Key *k2 = keyNew("user/tests/enum/valid2", KEY_VALUE, "FALSE", KEY_META, "check/enum", "'TRUE','FALSE'", KEY_END); 
	Key *k3 = keyNew("user/tests/enum/invalid1", KEY_VALUE, "BLA", KEY_META, "check/enum", "'TRUE','FALSE'", KEY_END);
	Key *k4 = keyNew("user/tests/enum/invalid2", KEY_VALUE, "", KEY_META, "check/enum", "'TRUE','FALSE'", KEY_END);
	KeySet *conf = ksNew(0, KS_END);
	KeySet *ks;
	PLUGIN_OPEN("enum");
	
	ks = ksNew(20, KS_END); 
	ksAppendKey(ks, k1);
	output_keyset(ks);
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel(ks);

	ks = ksNew(20, KS_END); 
	ksAppendKey(ks, k2);
	output_keyset(ks);
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel(ks);
	
	ks = ksNew(20, KS_END); 
	ksAppendKey(ks, k3);
	output_keyset(ks);
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel(ks);

	ks = ksNew(20, KS_END); 
	ksAppendKey(ks, k4);
	output_keyset(ks);
	ksRewind(ks);
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (-1), "kdbSet failed");
	ksDel(ks);
	
	keyDel(parentKey);
	PLUGIN_CLOSE();

}

			

int main(int argc, char** argv)
{
	printf ("ENUM     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test();

	printf ("\ntestmod_enum RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}

