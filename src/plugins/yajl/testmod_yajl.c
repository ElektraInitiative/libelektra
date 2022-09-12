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
#include "kdbconfig.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_internal.h>

// clang-format off
ElektraKeyset *getEmptyKeys(void)
{
	return ksNew(1,
			keyNew("user:/tests/yajl",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
			);
}

ElektraKeyset *getNullKeys(void)
{
	ElektraKey *k1, *k2;
	ElektraKeyset *ks = ksNew(10,
			k1 = keyNew("user:/tests/yajl/nullkey",
			       ELEKTRA_KEY_VALUE, "will be removed",
			       ELEKTRA_KEY_END),
			k2 = keyNew("user:/tests/yajl/second_nullkey",
			       ELEKTRA_KEY_VALUE, "will be removed too",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);
	keySetBinary(k1, NULL, 0);
	keySetBinary(k2, NULL, 0);

	ksRewind(ks); // shouldn't that be default?
	return ks;
}

ElektraKeyset *getBelowKeys(void)
{
	ElektraKeyset *ks = ksNew(10,
			keyNew("user:/tests/yajl/fancy/path/below/x/y/z",
			       ELEKTRA_KEY_VALUE, "val1",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/fancy/path/below/v/y/z",
			       ELEKTRA_KEY_VALUE, "val2",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);

	ksRewind(ks); // shouldn't that be default?
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

	ksRewind(ks); // shouldn't that be default?
	return ks;
}
*/

ElektraKeyset *getBooleanKeys(void)
{
	ElektraKeyset *ks = ksNew(10,
			keyNew("user:/tests/yajl/boolean_key",
			       ELEKTRA_KEY_VALUE, "1",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_boolean_key",
			       ELEKTRA_KEY_VALUE, "0",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);

	ksRewind(ks);
	return ks;
}

ElektraKeyset *getNumberKeys(void)
{
	ElektraKeyset *ks = ksNew(10,
			keyNew("user:/tests/yajl/number_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_number_key",
			       ELEKTRA_KEY_VALUE, "23002390202",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/third_number_key",
			       ELEKTRA_KEY_VALUE, "230020202.233",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);

	return ks;
}

ElektraKeyset *getStringKeys(void)
{
	ElektraKeyset *ks = ksNew(10,
			keyNew("user:/tests/yajl/string_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_string_key",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/third_string_key",
			       ELEKTRA_KEY_VALUE, "escape {}; \" \\ problem",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);

	return ks;
}

ElektraKeyset *getMapKeys (void)
{
	ElektraKeyset *ks = ksNew(10,
			keyNew("user:/tests/yajl/map/string_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/map/second_string_key",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/map/nested_map/string_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/map/nested_map/second_string_key",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_map/string_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_map/second_string_key",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/string_key",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/second_string_key",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END
		);

	return ks;
}

ElektraKeyset *getArrayKeys(void)
{
	ElektraKeyset *ks = ksNew(30,
			keyNew("user:/tests/yajl/array",
				ELEKTRA_KEY_META, "array", "#_12",
				ELEKTRA_KEY_META, "binary", "",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#0",
			       ELEKTRA_KEY_VALUE, "1",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#1",
			       ELEKTRA_KEY_VALUE, "25",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#2",
			       ELEKTRA_KEY_VALUE, "some string",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#3",
			       ELEKTRA_KEY_VALUE, "0",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#4",
			       ELEKTRA_KEY_VALUE, "1",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#5",
			       ELEKTRA_KEY_VALUE, "2",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#6",
			       ELEKTRA_KEY_VALUE, "3",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#7",
			       ELEKTRA_KEY_VALUE, "more \\ a",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#8",
			       ELEKTRA_KEY_VALUE, "string \"",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#9",
			       ELEKTRA_KEY_VALUE, "string abc",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#_10", // hack for keeping sort order
			       ELEKTRA_KEY_VALUE, "def abc",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#_11",
			       ELEKTRA_KEY_VALUE, "0",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			keyNew("user:/tests/yajl/array/#_12",
			       ELEKTRA_KEY_VALUE, "42",
			       ELEKTRA_KEY_META, "type", "double",
			       ELEKTRA_KEY_END),
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
			ELEKTRA_KS_END
		);

	return ks;
}

ElektraKeyset *getOpenICCKeys(void)
{
	ElektraKeyset *ks = ksNew(60,
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera",
	ELEKTRA_KEY_META, "array", "#1",
	ELEKTRA_KEY_META, "binary", "",
	ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/prefix",
		ELEKTRA_KEY_VALUE, "EXIF_",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_model",
		ELEKTRA_KEY_VALUE, "ShinyGlass", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_serial",
		ELEKTRA_KEY_VALUE, "1200000", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_mnft",
		ELEKTRA_KEY_VALUE, "GLAS", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/EXIF_manufacturer",
		ELEKTRA_KEY_VALUE, "Glasshuette", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/icc_profile",
		ELEKTRA_KEY_VALUE, "profile_name.icc",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/creation_date",
		ELEKTRA_KEY_VALUE, "05/08/11 11:59:50",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/expire_date",
		ELEKTRA_KEY_VALUE, "08/08/11 11:59:50",
		ELEKTRA_KEY_END),
//Typo in example:
//keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/automatic_assignment",
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/automatic_assigment",
		ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#0/comment",
		ELEKTRA_KEY_VALUE, "nonsense example", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/prefix",
		ELEKTRA_KEY_VALUE, "EXIF_", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_model",
		ELEKTRA_KEY_VALUE, "Knips", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_serial",
		ELEKTRA_KEY_VALUE, "3400000", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_mnft",
		ELEKTRA_KEY_VALUE, "CON", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/EXIF_manufacturer",
		ELEKTRA_KEY_VALUE, "ConquerLight",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/icc_profile",
		ELEKTRA_KEY_VALUE, "profile_name2.icc",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/creation_date",
		ELEKTRA_KEY_VALUE, "05/08/11 11:59:50",
		ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/expire_date",
		ELEKTRA_KEY_VALUE, "08/08/11 11:59:50",
		ELEKTRA_KEY_END),
// keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/automatic_assignment",
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/camera/#1/automatic_assigment",
		ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor",
	ELEKTRA_KEY_META, "array", "#1",
	ELEKTRA_KEY_META, "binary", "",
	ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/prefix",
		ELEKTRA_KEY_VALUE, "EDID_", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_mnft_id",
		ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_model_id",
		ELEKTRA_KEY_VALUE, "123", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_model",
		ELEKTRA_KEY_VALUE, "LCD1", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_serial",
		ELEKTRA_KEY_VALUE, "ABCD", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_red_x",
		ELEKTRA_KEY_VALUE, "0.599609", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_red_y",
		ELEKTRA_KEY_VALUE, "0.34375", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_green_x",
		ELEKTRA_KEY_VALUE, "0.320312", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_green_y",
		ELEKTRA_KEY_VALUE, "0.554688", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_blue_x",
		ELEKTRA_KEY_VALUE, "0.150391", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_blue_y",
		ELEKTRA_KEY_VALUE, "0.120117", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_white_x",
		ELEKTRA_KEY_VALUE, "0.313477", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_white_y",
		ELEKTRA_KEY_VALUE, "0.329102", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_gamma",
		ELEKTRA_KEY_VALUE, "2.2", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_mnft",
		ELEKTRA_KEY_VALUE, "VEN", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_manufacturer",
		ELEKTRA_KEY_VALUE, "Vendor1", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#0/EDID_date",
		ELEKTRA_KEY_VALUE, "2007-T16", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/prefix",
		ELEKTRA_KEY_VALUE, "EDID_", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_mnft_id",
		ELEKTRA_KEY_VALUE, "34", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_model_id",
		ELEKTRA_KEY_VALUE, "456", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_model",
		ELEKTRA_KEY_VALUE, "other monitor", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_serial",
		ELEKTRA_KEY_VALUE, "other serial", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_red_x",
		ELEKTRA_KEY_VALUE, "0.599609", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_red_y",
		ELEKTRA_KEY_VALUE, "0.34375", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_green_x",
		ELEKTRA_KEY_VALUE, "0.320312", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_green_y",
		ELEKTRA_KEY_VALUE, "0.554688", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_blue_x",
		ELEKTRA_KEY_VALUE, "0.150391", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_blue_y",
		ELEKTRA_KEY_VALUE, "0.120117", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_white_x",
		ELEKTRA_KEY_VALUE, "0.313477", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_white_y",
		ELEKTRA_KEY_VALUE, "0.329102", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_gamma",
		ELEKTRA_KEY_VALUE, "2.2", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_mnft",
		ELEKTRA_KEY_VALUE, "NEC", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_manufacturer",
		ELEKTRA_KEY_VALUE, "NEC", ELEKTRA_KEY_END),
keyNew("user:/tests/yajl/org/freedesktop/openicc/device/monitor/#1/EDID_date",
		ELEKTRA_KEY_VALUE, "2001-T12", ELEKTRA_KEY_END),


			ELEKTRA_KS_END
		);

	return ks;
}

ElektraKeyset *getSomeBelowKeys(void)
{
	return ksNew(10,
			keyNew("user:/some/path/below",
			       ELEKTRA_KEY_END),
			keyNew("user:/some/path/below/tests",
			       ELEKTRA_KEY_END),
			keyNew("user:/some/path/below/tests/yajl",
			       ELEKTRA_KEY_END),
			keyNew("user:/some/path/below/tests/yajl/boolean_key",
			       ELEKTRA_KEY_VALUE, "true",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			keyNew("user:/some/path/below/tests/yajl/second_boolean_key",
			       ELEKTRA_KEY_VALUE, "false",
			       ELEKTRA_KEY_META, "type", "boolean",
			       ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}

// clang-format on

ElektraKeyset * modules;

void test_json (const char * fileName, ElektraKeyset * compareKeySet, ElektraKeyset * conf)
{
	printf ("Test json with %s\n", srcdir_file (fileName));

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");
	// printf ("Test with %s\n", srcdir_file(fileName));

	ElektraKey * parentKey = keyNew ("user:/tests/yajl", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * keys = ksNew (0, ELEKTRA_KS_END);
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

void test_readWrite (const char * fileName, ElektraKeyset * conf)
{
	printf ("Test read write with %s\n", srcdir_file (fileName));

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");
	// printf ("Test with %s\n", srcdir_file(fileName));

	ElektraKey * parentKey = keyNew ("user:/tests/yajl", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * keys = ksNew (0, ELEKTRA_KS_END);
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

// TODO: make nicer and put to test framework
#define succeed_if_equal(x, y) succeed_if (!strcmp (x, y), x)

void test_nextNotBelow (void)
{
	printf ("Test next not below\n");

	ElektraKeyset * ks = getNullKeys ();
	ksRewind (ks);
	ElektraKey * k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/nullkey");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/nullkey");
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/second_nullkey");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/second_nullkey");
	k = elektraNextNotBelow (ks);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksCurrent (ks) == 0, "not at end of keyset");
	ksDel (ks);

	ks = getBooleanKeys ();
	ksRewind (ks);
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/boolean_key");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/boolean_key");
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/second_boolean_key");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/second_boolean_key");
	k = elektraNextNotBelow (ks);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksCurrent (ks) == 0, "not at end of keyset");
	ksDel (ks);

	ks = getBelowKeys ();
	ksRewind (ks);
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/fancy/path/below/v/y/z");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/fancy/path/below/v/y/z");
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/fancy/path/below/x/y/z");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/fancy/path/below/x/y/z");
	k = elektraNextNotBelow (ks);
	succeed_if (k == 0, "not at end of keyset");
	succeed_if (ksCurrent (ks) == 0, "not at end of keyset");
	ksDel (ks);

	ks = getMapKeys ();
	ksRewind (ks);
	k = elektraNextNotBelow (ks);
	succeed_if_equal (keyName (k), "user:/tests/yajl/map/nested_map/second_string_key");
	succeed_if_equal (keyName (ksCurrent (ks)), "user:/tests/yajl/map/nested_map/second_string_key");
	ksDel (ks);
}

void test_reverseLevel (void)
{
	ElektraKey * k = keyNew ("user:/abc/defghi/jkl", ELEKTRA_KEY_END);
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
	ElektraKey * k = keyNew ("user:///", ELEKTRA_KEY_END);
	succeed_if (elektraKeyCountLevel (k) == 1, "count level wrong");
	keySetName (k, "user:/x");
	succeed_if (elektraKeyCountLevel (k) == 2, "count level wrong");
	keySetName (k, "user:/x/z/f");
	succeed_if (elektraKeyCountLevel (k) == 4, "count level wrong");
	keySetName (k, "user:/x/z\\/f");
	succeed_if (elektraKeyCountLevel (k) == 3, "count level wrong");

	ElektraKey * k2 = keyNew ("user:/x/z", ELEKTRA_KEY_END);
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
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * parentKey = keyNew ("user:/tests/yajl", ELEKTRA_KEY_VALUE, "/proc/self/fd/1", ELEKTRA_KEY_END);

	Plugin * plugin = elektraPluginOpen ("yajl", modules, conf, 0);
	exit_if_fail (plugin != 0, "could not open plugin");

	ElektraKeyset * ks = getNullKeys ();
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

	modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);

	init (argc, argv);

	test_nextNotBelow ();
	test_reverseLevel ();
	test_countLevel ();
	test_writing ();

	test_json ("yajl/testdata_null.json", getNullKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_boolean.json", getBooleanKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_number.json", getNumberKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_string.json", getStringKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_maps.json", getMapKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_array.json", getArrayKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/testdata_below.json", getBelowKeys (), ksNew (0, ELEKTRA_KS_END));
	test_json ("yajl/OpenICC_device_config_DB.json", getOpenICCKeys (), ksNew (0, ELEKTRA_KS_END));

	// TODO currently do not have a KeySet, wait for C-plugin to make
	// it easy to generate it..
	test_readWrite ("yajl/empty_object.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/empty_array.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/top_level_string.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/top_level_integer.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/rfc_object.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/rfc_array.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_mixed.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_in_array.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_in_array_anon_map.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_nested.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_broken.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_special_ending.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_outside.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/keyframes_complex.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_mixed2.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_special_start.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_array_mixed3.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_empty_in_array.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_empty_in_map.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_empty_in_array1.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_empty_in_map2.json", ksNew (0, ELEKTRA_KS_END));
	test_readWrite ("yajl/testdata_empty_in_map1.json", ksNew (0, ELEKTRA_KS_END));

	elektraModulesClose (modules, 0);
	ksDel (modules);

	print_result ("test_yajl");

	return nbError;
}
