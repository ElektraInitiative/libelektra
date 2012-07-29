/*************************************************************************** 
 *           test_mount.c  - Test suite for testing backend mounting
 *                  -------------------
 *  begin                : Thu Nov 6 2007
 *  copyright            : (C) 2007 by Patrick Sabin
 *  email                : patricksabin@gmx.at
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_internal.h>

KeySet *getEmptyKeys()
{
	return ksNew(1,
			keyNew("user",
			       KEY_END),
			KS_END
			);
}

KeySet *getBooleanKeys()
{
	KeySet *ks = ksNew(10,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			keyNew("user/tests/yajl/boolean_key",
			       KEY_VALUE, "true",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user/tests/yajl/second_boolean_key",
			       KEY_VALUE, "false",
			       KEY_META, "type", "boolean",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getNullKeys()
{
	Key *k1, *k2;
	KeySet *ks = ksNew(10,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			k1 = keyNew("user/tests/yajl/nullkey",
			       KEY_VALUE, "will be removed",
			       KEY_END),
			k2 = keyNew("user/tests/yajl/second_nullkey",
			       KEY_VALUE, "will be removed too",
			       KEY_END),
			KS_END
		);
	keySetBinary(k1, NULL, 0);
	keySetBinary(k2, NULL, 0);

	return ks;
}

KeySet *modules;

void test_parse_json(const char* fileName, KeySet * compareKeySet)
{
	KeySet *conf = ksNew(0);
	Plugin *plugin = elektraPluginOpen("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");

	Key *parentKey = keyNew ("user/tests/yajl",
			KEY_VALUE, srcdir_file(fileName),
			KEY_END);
	KeySet *keys = ksNew(0);
	succeed_if (plugin->kdbGet(plugin, keys, parentKey) == 1, "kdbGet was not successful");

	succeed_if (compare_keyset(keys, compareKeySet) == 0, "keyset is not like it should be");

	output_errors(parentKey);
	output_warnings(parentKey);

	output_keyset(keys);
	output_keyset(compareKeySet);

	keyDel (parentKey);
	ksDel (keys);
	ksDel (compareKeySet);

	elektraPluginClose(plugin, 0);
}

int main(int argc, char** argv)
{
	printf("YAJL       TESTS\n");
	printf("==================\n\n");

	modules = ksNew(0);
	elektraModulesInit(modules, 0);

	init (argc, argv);

	test_parse_json("examples/testdata_empty.js", getEmptyKeys());
	test_parse_json("examples/testdata_null.js", getNullKeys());
	test_parse_json("examples/testdata_boolean.js", getBooleanKeys());

	elektraModulesClose(modules, 0);
	ksDel (modules);

	printf("\ntest_mount RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

