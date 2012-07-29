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

KeySet *getNumberKeys()
{
	KeySet *ks = ksNew(10,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			keyNew("user/tests/yajl/number_key",
			       KEY_VALUE, "25",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/second_number_key",
			       KEY_VALUE, "23002390202",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/third_number_key",
			       KEY_VALUE, "230020202.233",
			       KEY_META, "type", "number",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getStringKeys()
{
	KeySet *ks = ksNew(10,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			keyNew("user/tests/yajl/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user/tests/yajl/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user/tests/yajl/third_string_key",
			       KEY_VALUE, "escape {}; \" \\ problem",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getMapKeys ()
{
	KeySet *ks = ksNew(10,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			keyNew("user/tests/yajl/map",
			       KEY_END),
			keyNew("user/tests/yajl/map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user/tests/yajl/map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user/tests/yajl/map/nested_map",
			       KEY_END),
			keyNew("user/tests/yajl/map/nested_map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user/tests/yajl/map/nested_map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user/tests/yajl/second_map",
			       KEY_END),
			keyNew("user/tests/yajl/second_map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user/tests/yajl/second_map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user/tests/yajl/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user/tests/yajl/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getArrayKeys()
{
	KeySet *ks = ksNew(30,
			keyNew("user",
			       KEY_END),
			keyNew("user/tests",
			       KEY_END),
			keyNew("user/tests/yajl",
			       KEY_END),
			keyNew("user/tests/yajl/array",
			       KEY_END),
			keyNew("user/tests/yajl/array/0",
			       KEY_VALUE, "true",
			       KEY_META, "array", "",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user/tests/yajl/array/1",
			       KEY_VALUE, "25",
			       KEY_META, "array", "",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/array/2",
			       KEY_META, "array", "",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user/tests/yajl/array/3",
			       KEY_META, "array", "",
			       KEY_VALUE, "0",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/array/4",
			       KEY_META, "array", "",
			       KEY_VALUE, "1",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/array/5",
			       KEY_META, "array", "",
			       KEY_VALUE, "2",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/array/6",
			       KEY_META, "array", "",
			       KEY_VALUE, "3",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user/tests/yajl/array/7",
			       KEY_META, "array", "",
			       KEY_VALUE, "more \\ a",
			       KEY_END),
			keyNew("user/tests/yajl/array/8",
			       KEY_META, "array", "",
			       KEY_VALUE, "string \"",
			       KEY_END),
			keyNew("user/tests/yajl/array/9",
			       KEY_META, "array", "",
			       KEY_VALUE, "string abc",
			       KEY_END),
			keyNew("user/tests/yajl/array/10",
			       KEY_META, "array", "",
			       KEY_VALUE, "def abc",
			       KEY_END),
			keyNew("user/tests/yajl/array/11",
			       KEY_META, "array", "",
			       KEY_VALUE, "false",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user/tests/yajl/array/12",
			       KEY_META, "array", "",
			       KEY_VALUE, "42",
			       KEY_META, "type", "number",
			       KEY_END),
			KS_END
		);

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

	printf ("The keys we read out are:\n");
	output_keyset(keys);
	printf ("The keys we compared it with:\n");
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

	// test_parse_json("examples/testdata_empty.json", getEmptyKeys());
	// test_parse_json("examples/testdata_null.json", getNullKeys());
	// test_parse_json("examples/testdata_boolean.json", getBooleanKeys());
	// test_parse_json("examples/testdata_number.json", getNumberKeys());
	// test_parse_json("examples/testdata_string.json", getStringKeys());
	// test_parse_json("examples/testdata_maps.json", getMapKeys());
	test_parse_json("examples/testdata_array.json", getArrayKeys());

	elektraModulesClose(modules, 0);
	ksDel (modules);

	printf("\ntest_yajl RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

