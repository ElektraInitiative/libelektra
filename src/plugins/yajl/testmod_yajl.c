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

KeySet * get_null()
{
	Key *k1, *k2;
	KeySet *ks = ksNew(10,
			keyNew("user/tests/yajl",
			       KEY_VALUE, "root key",
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

void test_readnull()
{
	KeySet *conf = ksNew(0);
	Plugin *plugin = elektraPluginOpen("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");

	Key *parentKey = keyNew ("user/tests/yajl",
			KEY_VALUE, srcdir_file("testdata_null.js"),
			KEY_END);
	KeySet *keys = ksNew(0);
	succeed_if (plugin->kdbGet(plugin, keys, parentKey) == 1, "kdbGet was not successful");

	output_errors(parentKey);
	output_warnings(parentKey);
	output_keyset(keys);

	elektraPluginClose(plugin, 0);

}

int main(int argc, char** argv)
{
	printf("YAJL       TESTS\n");
	printf("==================\n\n");

	modules = ksNew(0);
	elektraModulesInit(modules, 0);

	init (argc, argv);

	test_readnull();

	elektraModulesClose(modules, 0);

	printf("\ntest_mount RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

