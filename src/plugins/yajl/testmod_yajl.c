/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "iterator.h"
#include "name.h"
#include "yajl.h"

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_internal.h>

// clang-format off
KeySet *getEmptyKeys(void)
{
	return ksNew(1,
			keyNew("user:/tests/yajl",
			       KEY_END),
			KS_END
			);
}

KeySet *getNullKeys(void)
{
	Key *k1, *k2;
	KeySet *ks = ksNew(10,
			k1 = keyNew("user:/tests/yajl/nullkey",
			       KEY_VALUE, "will be removed",
			       KEY_END),
			k2 = keyNew("user:/tests/yajl/second_nullkey",
			       KEY_VALUE, "will be removed too",
			       KEY_END),
			KS_END
		);
	keySetBinary(k1, NULL, 0);
	keySetBinary(k2, NULL, 0);

	return ks;
}

KeySet *getBelowKeys(void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl/fancy/path/below/x/y/z",
			       KEY_VALUE, "val1",
			       KEY_END),
			keyNew("user:/tests/yajl/fancy/path/below/v/y/z",
			       KEY_VALUE, "val2",
			       KEY_END),
			KS_END
		);

	return ks;
}

/*
KeySet *getBelowKeys(void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl",
			       KEY_END),
			keyNew("user:/tests/yajl/x",
			       KEY_END),
			keyNew("user:/tests/yajl/x/y",
			       KEY_END),
			keyNew("user:/tests/yajl/x/y/z",
			       KEY_VALUE, "val1",
			       KEY_END),
			keyNew("user:/tests/yajl/v",
			       KEY_END),
			keyNew("user:/tests/yajl/v/y",
			       KEY_END),
			keyNew("user:/tests/yajl/v/y/z",
			       KEY_VALUE, "val2",
			       KEY_END),
			KS_END
		);

	return ks;
}
*/

KeySet *getBooleanKeys(void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl/boolean_key",
			       KEY_VALUE, "1",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user:/tests/yajl/second_boolean_key",
			       KEY_VALUE, "0",
			       KEY_META, "type", "boolean",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getNumberKeys(void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl/number_key",
			       KEY_VALUE, "25",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/second_number_key",
			       KEY_VALUE, "23002390202",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/third_number_key",
			       KEY_VALUE, "230020202.233",
			       KEY_META, "type", "double",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getStringKeys(void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user:/tests/yajl/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user:/tests/yajl/third_string_key",
			       KEY_VALUE, "escape {}; \" \\ problem",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getMapKeys (void)
{
	KeySet *ks = ksNew(10,
			keyNew("user:/tests/yajl/map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user:/tests/yajl/map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user:/tests/yajl/map/nested_map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user:/tests/yajl/map/nested_map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user:/tests/yajl/second_map/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user:/tests/yajl/second_map/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user:/tests/yajl/string_key",
			       KEY_VALUE, "25",
			       KEY_END),
			keyNew("user:/tests/yajl/second_string_key",
			       KEY_VALUE, "some string",
			       KEY_END),
			KS_END
		);

	return ks;
}

KeySet *getArrayKeys(void)
{
	KeySet *ks = ksNew(30,
			keyNew("user:/tests/yajl/array",
				KEY_META, "array", "#_12",
				KEY_META, "binary", "",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#0",
			       KEY_VALUE, "1",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#1",
			       KEY_VALUE, "25",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#2",
			       KEY_VALUE, "some string",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#3",
			       KEY_VALUE, "0",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#4",
			       KEY_VALUE, "1",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#5",
			       KEY_VALUE, "2",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#6",
			       KEY_VALUE, "3",
			       KEY_META, "type", "double",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#7",
			       KEY_VALUE, "more \\ a",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#8",
			       KEY_VALUE, "string \"",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#9",
			       KEY_VALUE, "string abc",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#_10", // hack for keeping sort order
			       KEY_VALUE, "def abc",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#_11",
			       KEY_VALUE, "0",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#_12",
			       KEY_VALUE, "42",
			       KEY_META, "type", "double",
			       KEY_END),
			/*
			keyNew("user:/tests/yajl/array/#___333",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#_#__#333",
			       KEY_VALUE, "42",
			       KEY_META, "type", "number",
			       KEY_END),
			keyNew("user:/tests/yajl/array/#______4444", // number of _ not intuitive
			       KEY_END),
			keyNew("user:/tests/yajl/array/#_#__#___#4444", // gets quite long... (but works!)
			       KEY_END),
			*/
			KS_END
		);

	return ks;
}

KeySet *getOpenICCKeys(void)
{
	KeySet *ks = ksNew(60,
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera",
	KEY_META, "array", "#1",
	KEY_META, "binary", "",
	KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/prefix",
		KEY_VALUE, "EXIF_",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_model",
		KEY_VALUE, "ShinyGlass", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_serial",
		KEY_VALUE, "1200000", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_mnft",
		KEY_VALUE, "GLAS", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_manufacturer",
		KEY_VALUE, "Glasshuette", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/icc_profile",
		KEY_VALUE, "profile_name.icc",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/creation_date",
		KEY_VALUE, "05/08/11 11:59:50",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/expire_date",
		KEY_VALUE, "08/08/11 11:59:50",
		KEY_END),
//Typo in example:
//keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/automatic_assignment",
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/automatic_assigment",
		KEY_VALUE, "1", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/comment",
		KEY_VALUE, "nonsense example", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/prefix",
		KEY_VALUE, "EXIF_", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_model",
		KEY_VALUE, "Knips", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_serial",
		KEY_VALUE, "3400000", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_mnft",
		KEY_VALUE, "CON", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_manufacturer",
		KEY_VALUE, "ConquerLight",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/icc_profile",
		KEY_VALUE, "profile_name2.icc",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/creation_date",
		KEY_VALUE, "05/08/11 11:59:50",
		KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/expire_date",
		KEY_VALUE, "08/08/11 11:59:50",
		KEY_END),
// keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/automatic_assignment",
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/automatic_assigment",
		KEY_VALUE, "1", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor",
	KEY_META, "array", "#1",
	KEY_META, "binary", "",
	KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/prefix",
		KEY_VALUE, "EDID_", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_mnft_id",
		KEY_VALUE, "12", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_model_id",
		KEY_VALUE, "123", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_model",
		KEY_VALUE, "LCD1", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_serial",
		KEY_VALUE, "ABCD", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_red_x",
		KEY_VALUE, "0.599609", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_red_y",
		KEY_VALUE, "0.34375", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_green_x",
		KEY_VALUE, "0.320312", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_green_y",
		KEY_VALUE, "0.554688", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_blue_x",
		KEY_VALUE, "0.150391", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_blue_y",
		KEY_VALUE, "0.120117", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_white_x",
		KEY_VALUE, "0.313477", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_white_y",
		KEY_VALUE, "0.329102", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_gamma",
		KEY_VALUE, "2.2", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_mnft",
		KEY_VALUE, "VEN", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_manufacturer",
		KEY_VALUE, "Vendor1", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_date",
		KEY_VALUE, "2007-T16", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/prefix",
		KEY_VALUE, "EDID_", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_mnft_id",
		KEY_VALUE, "34", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_model_id",
		KEY_VALUE, "456", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_model",
		KEY_VALUE, "other monitor", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_serial",
		KEY_VALUE, "other serial", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_red_x",
		KEY_VALUE, "0.599609", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_red_y",
		KEY_VALUE, "0.34375", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_green_x",
		KEY_VALUE, "0.320312", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_green_y",
		KEY_VALUE, "0.554688", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_blue_x",
		KEY_VALUE, "0.150391", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_blue_y",
		KEY_VALUE, "0.120117", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_white_x",
		KEY_VALUE, "0.313477", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_white_y",
		KEY_VALUE, "0.329102", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_gamma",
		KEY_VALUE, "2.2", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_mnft",
		KEY_VALUE, "NEC", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_manufacturer",
		KEY_VALUE, "NEC", KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_date",
		KEY_VALUE, "2001-T12", KEY_END),


			KS_END
		);

	return ks;
}

KeySet *getSomeBelowKeys(void)
{
	return ksNew(10,
			keyNew("user:/some/path/below",
			       KEY_END),
			keyNew("user:/some/path/below/tests",
			       KEY_END),
			keyNew("user:/some/path/below/tests/yajl",
			       KEY_END),
			keyNew("user:/some/path/below/tests/yajl/boolean_key",
			       KEY_VALUE, "true",
			       KEY_META, "type", "boolean",
			       KEY_END),
			keyNew("user:/some/path/below/tests/yajl/second_boolean_key",
			       KEY_VALUE, "false",
			       KEY_META, "type", "boolean",
			       KEY_END),
			KS_END);
}

// clang-format on

KeySet * modules;

void test_json (const char * fileName, KeySet * compareKeySet, KeySet * conf)
{
	printf ("Test json with %s\n", srcdir_file (fileName));

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");
	// printf ("Test with %s\n", srcdir_file(fileName));

	Key * parentKey = keyNew ("user:/tests/yajl", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * keys = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, keys, parentKey) == 1, "kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/*
	output_keyset(keys);
	output_keyset(compareKeySet);
	*/
	compare_keyset (keys, compareKeySet);

	keySetString (parentKey, elektraFilename ());
	// printf("File name is: %s\n", keyString(parentKey));

	succeed_if (plugin->kdbSet (plugin, keys, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");
	elektraUnlink (keyString (parentKey));

	/*
	printf ("The keys we read out are:\n");
	output_keyset(keys);
	printf ("The keys we compared it with:\n");
	output_keyset(compareKeySet);
	*/

	keyDel (parentKey);
	ksDel (keys);
	ksDel (compareKeySet);

	elektraPluginClose (plugin, 0);
}

void test_readWrite (const char * fileName, KeySet * conf)
{
	printf ("Test read write with %s\n", srcdir_file (fileName));

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");
	// printf ("Test with %s\n", srcdir_file(fileName));

	Key * parentKey = keyNew ("user:/tests/yajl", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * keys = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, keys, parentKey) == 1, "kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	// output_keyset(keys);

	keySetString (parentKey, elektraFilename ());
	// keySetString(parentKey, "/proc/self/fd/1");
	// printf("File name is: %s\n", keyString(parentKey));

	succeed_if (plugin->kdbSet (plugin, keys, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");
	elektraUnlink (keyString (parentKey));

	keyDel (parentKey);
	ksDel (keys);

	elektraPluginClose (plugin, 0);
}

void test_writeMetaMustFail (const char * fileName, KeySet * conf)
{
	printf ("Test write unknown meta keys must fail with %s\n", srcdir_file (fileName));

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");
	// printf ("Test with %s\n", srcdir_file(fileName));

	Key * parentKey = keyNew ("user:/tests/yajl", KEY_VALUE, "asdf", KEY_META, "asdf", "asdf", KEY_END);
	KeySet * keys = ksNew (1, keyNew ("user:/tests/yajl/asdf", KEY_VALUE, "asdf", KEY_META, "asdf", "asdf", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, keys, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "kdbSet did not error on bogus meta key insertion");

	elektraUnlink (keyString (parentKey));

	keyDel (parentKey);
	ksDel (keys);

	elektraPluginClose (plugin, 0);
}

// TODO: make nicer and put to test framework
#define succeed_if_equal(x, y) succeed_if (!strcmp (x, y), x)

void test_nextNotBelow (void)
{
	printf ("Test next not below\n");
	KeySet * ks;
	Key * k;

	ks = getNullKeys ();
	k = elektraNextNotBelow (ks, 0);
	succeed_if_equal (keyName (k), "user:/tests/yajl/nullkey");
	succeed_if_equal (keyName (ksAtCursor (ks, 0)), "user:/tests/yajl/nullkey");

	k = elektraNextNotBelow (ks, 1);
	succeed_if_equal (keyName (k), "user:/tests/yajl/second_nullkey");
	succeed_if_equal (keyName (ksAtCursor (ks, 1)), "user:/tests/yajl/second_nullkey");

	k = elektraNextNotBelow (ks, 2);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksAtCursor (ks, 2) == 0, "not at end of keyset");
	ksDel (ks);


	ks = getBooleanKeys ();
	k = elektraNextNotBelow (ks, 0);
	succeed_if_equal (keyName (k), "user:/tests/yajl/boolean_key");
	succeed_if_equal (keyName (ksAtCursor (ks, 0)), "user:/tests/yajl/boolean_key");

	k = elektraNextNotBelow (ks, 1);
	succeed_if_equal (keyName (k), "user:/tests/yajl/second_boolean_key");
	succeed_if_equal (keyName (ksAtCursor (ks, 1)), "user:/tests/yajl/second_boolean_key");

	k = elektraNextNotBelow (ks, 2);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksAtCursor (ks, 2) == 0, "not at end of keyset");
	ksDel (ks);


	ks = getBelowKeys ();
	k = elektraNextNotBelow (ks, 0);
	succeed_if_equal (keyName (k), "user:/tests/yajl/fancy/path/below/v/y/z");
	succeed_if_equal (keyName (ksAtCursor (ks, 0)), "user:/tests/yajl/fancy/path/below/v/y/z");

	k = elektraNextNotBelow (ks, 1);
	succeed_if_equal (keyName (k), "user:/tests/yajl/fancy/path/below/x/y/z");
	succeed_if_equal (keyName (ksAtCursor (ks, 1)), "user:/tests/yajl/fancy/path/below/x/y/z");

	k = elektraNextNotBelow (ks, 2);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksAtCursor (ks, 2) == 0, "not at end of keyset");
	ksDel (ks);


	ks = getMapKeys ();
	k = elektraNextNotBelow (ks, 0);
	succeed_if_equal (keyName (k), "user:/tests/yajl/map/nested_map/second_string_key");
	succeed_if_equal (keyName (ksAtCursor (ks, 0)), "user:/tests/yajl/map/nested_map/second_string_key");
	ksDel (ks);
}

void test_reverseLevel (void)
{
	Key * k = keyNew ("user:/abc/defghi/jkl", KEY_END);
	int level = 0;
	char buffer[20];

	printf ("Test reverse level\n");

	keyNameReverseIterator it = elektraKeyNameGetReverseIterator (k);
	while (elektraKeyNameReverseNext (&it))
	{
		level++;

		strncpy (buffer, it.current, it.size);
		buffer[it.size] = 0;

		// printf("Level %d name: \"%s\"\n",level,buffer);
		switch (level)
		{
		case 4:
			succeed_if_same_string (buffer, "user:");
			break;
		case 3:
			succeed_if_same_string (buffer, "abc");
			break;
		case 2:
			succeed_if_same_string (buffer, "defghi");
			break;
		case 1:
			succeed_if_same_string (buffer, "jkl");
			break;
		default:
			succeed_if (0, "should not reach case statement");
		}
	}

	keySetName (k, "user:////\\/abc/\\/def\\/ghi////jkl\\/\\/");

	level = 0;
	it = elektraKeyNameGetReverseIterator (k);
	while (elektraKeyNameReverseNext (&it))
	{
		level++;

		strncpy (buffer, it.current, it.size);
		buffer[it.size] = 0;

		// printf("Level %d name: \"%s\"\n",level,buffer);
		switch (level)
		{
		case 4:
			succeed_if (strcmp (buffer, "user:") == 0, "keyNameGetOneLevel not correct");
			break;
		case 3:
			succeed_if (strcmp (buffer, "\\/abc") == 0, "keyNameGetOneLevel not correct");
			break;
		case 2:
			succeed_if (strcmp (buffer, "\\/def\\/ghi") == 0, "keyNameGetOneLevel not correct");
			break;
		case 1:
			succeed_if (strcmp (buffer, "jkl\\/\\/") == 0, "keyNameGetOneLevel not correct");
			break;
		default:
			succeed_if (0, "should not reach case statement");
		}
	}

	keyDel (k);
}

void test_countLevel (void)
{
	Key * k = keyNew ("user:///", KEY_END);
	succeed_if (elektraKeyCountLevel (k) == 1, "count level wrong");
	keySetName (k, "user:/x");
	succeed_if (elektraKeyCountLevel (k) == 2, "count level wrong");
	keySetName (k, "user:/x/z/f");
	succeed_if (elektraKeyCountLevel (k) == 4, "count level wrong");
	keySetName (k, "user:/x/z\\/f");
	succeed_if (elektraKeyCountLevel (k) == 3, "count level wrong");

	Key * k2 = keyNew ("user:/x/z", KEY_END);
	succeed_if (elektraKeyCountEqualLevel (k, k2) == 2, "equal level wrong");

	keySetName (k, "user:/x/z\\/f");
	keySetName (k2, "user:/x/z\\/f");
	succeed_if (elektraKeyCountEqualLevel (k, k2) == 3, "equal level wrong");

	keySetName (k, "user:/x/v/ffkkk");
	keySetName (k2, "user:/x/v/ff");
	succeed_if (elektraKeyCountEqualLevel (k, k2) == 3, "equal level wrong");

	keySetName (k, "user:/x/v/ff");
	keySetName (k2, "user:/x/v/ff");
	succeed_if (elektraKeyCountEqualLevel (k, k2) == 4, "equal level wrong");

	keySetName (k, "user:/x\\abc/v/ff");
	keySetName (k2, "user:/x\\abc/v/ff");
	succeed_if (elektraKeyCountEqualLevel (k, k2) == 4, "equal level wrong");

	keyDel (k);
	keyDel (k2);
}

void test_writing (void)
{
	KeySet * conf = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user:/tests/yajl", KEY_VALUE, "/proc/self/fd/1", KEY_END);

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");

	KeySet * ks = getNullKeys ();
	/*
	output_keyset(ks);

	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if(plugin->kdbSet(plugin, getBooleanKeys(), parentKey) == 1, "kdbSet was not successful");
	succeed_if(plugin->kdbSet(plugin, getNumberKeys(), parentKey) == 1, "kdbSet was not successful");
	succeed_if(plugin->kdbSet(plugin, getStringKeys(), parentKey) == 1, "kdbSet was not successful");
	succeed_if(plugin->kdbSet(plugin, getMapKeys(), parentKey) == 1, "kdbSet was not successful");
	succeed_if(plugin->kdbSet(plugin, getArrayKeys(), parentKey) == 1, "kdbSet was not successful");
	ksDel(ks); ks = getOpenICCKeys();
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == 1, "kdbSet was not successful");
	*/

	ksDel (ks);
	keyDel (parentKey);

	elektraPluginClose (plugin, 0);
}

int main (int argc, char ** argv)
{
	printf ("YAJL       TESTS\n");
	printf ("==================\n\n");

	modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	init (argc, argv);


	test_nextNotBelow ();
	test_reverseLevel ();
	test_countLevel ();
	test_writing ();

	test_json ("yajl/testdata_null.json", getNullKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_boolean.json", getBooleanKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_number.json", getNumberKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_string.json", getStringKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_maps.json", getMapKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_array.json", getArrayKeys (), ksNew (0, KS_END));
	test_json ("yajl/testdata_below.json", getBelowKeys (), ksNew (0, KS_END));
	test_json ("yajl/OpenICC_device_config_DB.json", getOpenICCKeys (), ksNew (0, KS_END));

	// TODO currently do not have a KeySet, wait for C-plugin to make
	// it easy to generate it..
	test_readWrite ("yajl/empty_object.json", ksNew (0, KS_END));
	test_readWrite ("yajl/empty_array.json", ksNew (0, KS_END));
	test_readWrite ("yajl/top_level_string.json", ksNew (0, KS_END));
	test_readWrite ("yajl/top_level_integer.json", ksNew (0, KS_END));
	test_readWrite ("yajl/rfc_object.json", ksNew (0, KS_END));
	test_readWrite ("yajl/rfc_array.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_mixed.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_in_array.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_in_array_anon_map.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_nested.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_broken.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_special_ending.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_outside.json", ksNew (0, KS_END));
	test_readWrite ("yajl/keyframes_complex.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_mixed2.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_special_start.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_array_mixed3.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_empty_in_array.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_empty_in_map.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_empty_in_array1.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_empty_in_map2.json", ksNew (0, KS_END));
	test_readWrite ("yajl/testdata_empty_in_map1.json", ksNew (0, KS_END));

	test_writeMetaMustFail ("yajl/testdata_write_meta.json", ksNew (0, KS_END));

	elektraModulesClose (modules, 0);
	ksDel (modules);

	print_result ("test_yajl");

	return nbError;
}
