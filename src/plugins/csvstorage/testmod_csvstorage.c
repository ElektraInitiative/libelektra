/**
 * \file
 *
 * \brief Tests for csvstorage plugin
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void testread(const char *file)
{
	printf("testing on %s:%s\n", srcdir_file(file), file);
	Key * parentKey = keyNew ("user/tests/csvstorage", KEY_VALUE, srcdir_file(file), KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/useheader", KEY_VALUE, "1", KEY_END), KS_END);
	PLUGIN_OPEN("csvstorage");
	KeySet *ks = ksNew(0, KS_END);
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	output_keyset(ks);
	Key *key;
	key = ksLookupByName(ks, "user/tests/csvstorage/#1/col1", 0);
	exit_if_fail(key, "key not found");
	succeed_if(strcmp(keyString(key), "l1c1") == 0, "wrong key");
	key = ksLookupByName(ks, "user/tests/csvstorage/#2/col2", 0);
	exit_if_fail(key, "key not found");
	succeed_if(strcmp(keyString(key), "l2c2") == 0, "wrong key");

	ksDel(ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

static void testreadwriteinvalid(const char *file)
{

	Key * parentKey = keyNew ("user/tests/csvstorage", KEY_VALUE, srcdir_file(file), KEY_END);
	KeySet *conf = 0;
	printf("%s\n", srcdir_file(file));
	KeySet *ks = ksNew(0, KS_END);
	PLUGIN_OPEN("csvstorage");
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >0,  "call to kdbGet was not successful");
	succeed_if(!output_warnings(parentKey), "no warnings in kdbGet");	
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}


static void testwriteinvalidheader(const char *file)
{

	Key * parentKey = keyNew ("user/tests/csvstorage", KEY_VALUE, srcdir_file(file), KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/useheader", KEY_VALUE, "1", KEY_END), KS_END);

	KeySet *ks = ksNew(0, KS_END);
	PLUGIN_OPEN("csvstorage");
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >0,  "call to kdbGet was not successful");
	succeed_if(!output_warnings(parentKey), "no warnings in kdbGet");	
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}

static void testwritevalidemptycol(const char *file)
{

	Key * parentKey = keyNew ("user/tests/csvstorage", KEY_VALUE, srcdir_file(file), KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/useheader", KEY_VALUE, "1", KEY_END), KS_END);

	printf("%s\n", srcdir_file(file));
	KeySet *ks = ksNew(0, KS_END);
	PLUGIN_OPEN("csvstorage");
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >0,  "call to kdbGet was not successful");
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) >= 0, "error: couldn't write data");
	ksDel(ks);
	keyDel(parentKey);
	PLUGIN_CLOSE();
}
int main(int argc, char** argv)
{
	printf ("CSVSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testread("csvstorage/valid.csv");
	testreadwriteinvalid("csvstorage/invalid_columns.csv");
	testwriteinvalidheader("csvstorage/invalid_columns_header2.csv");
	testwritevalidemptycol("csvstorage/valid_empty_col.csv");


	printf ("\ntestmod_csvstorage RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}

