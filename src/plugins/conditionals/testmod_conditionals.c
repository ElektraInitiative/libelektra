/**
 * @file
 *
 * @brief Tests for conditionals plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_ifthenelseint()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(totest== '153') ? (bla/val1 == '100') : (bla/val2 == '100')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}
static void test_ifthenint()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(     totest   !=     '15'  ) ? (bla/val1 == '100')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}
static void test_ifthenltint()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(     totest   <    '153'    ) ? (bla/val1 == '100')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}
static void test_ifthengtint()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(totest>'153') ? (bla/val1 == '100') : (bla/val2 <= '1')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == -1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}

static void test_ifthenkey()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(totest>bla/val3) ? (bla/val1 == '100') :  (bla/val2 <= '1')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}


static void test_emptyisempty()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition", "(totest=='') ? (bla/val1 == '100') : (bla/val2 <= '1')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}


static void test_notempty()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition", "(totest!='') ? (bla/val1 == '100') : (bla/val2 <= '1')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}

static void test_ifsetthenval()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition", "(totest=='') ? (totest := 'BLA')", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	Key *key = ksLookupByName(ks, "user/tests/conditionals/totest", 0);
	succeed_if(strcmp(keyString(key), "BLA") == 0, "error setting then value");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();

}
static void test_ifsetthenkey()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition", "(totest=='') ? (totest := bla/val1)", KEY_END),
	keyNew("user/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
	keyNew("user/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
	keyNew("user/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	Key *key = ksLookupByName(ks, "user/tests/conditionals/totest", 0);
	succeed_if(strcmp(keyString(key), "100") == 0, "error setting then value");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}
static void test_assignThen()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition", "(totest=='Hello') ? ('World')", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	Key *key = ksLookupByName(ks, "user/tests/conditionals/totest", 0);
	succeed_if(strcmp(keyString(key), "World") == 0, "error setting then value");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}

static void test_assignElse()
{
	Key *parentKey = keyNew("user/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet *ks = ksNew(5,
	keyNew("user/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition", "(totest=='Hell') ? ('World') : ('Fail')", KEY_END),
		KS_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("conditionals");
	ksRewind(ks);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == 1, "error");
	Key *key = ksLookupByName(ks, "user/tests/conditionals/totest", 0);
	succeed_if(strcmp(keyString(key), "Fail") == 0, "error setting then value");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}
int main(int argc, char** argv)
{
	printf ("CONDITIONALS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ifthenelseint();
	test_ifthenint();
	test_ifthenltint();
	test_ifthengtint();
	test_ifthenkey();
	test_emptyisempty();
	test_notempty();
	test_ifsetthenval();
	test_ifsetthenkey();
	test_assignThen();
	test_assignElse();
	printf ("\ntestmod_conditionals RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}

