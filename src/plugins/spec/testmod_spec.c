/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include "spec.h"

#include "kdb.h"

#include <tests_plugin.h>

#include <stdio.h>

#ifndef PARENT_KEY
#define PARENT_KEY "/sw/org"
#endif

#ifndef PARENT_KEY_WITH_NAMESPACE
#define PARENT_KEY_WITH_NAMESPACE "user:/sw/org"
#endif

#define TEST_BEGIN                                                                                                                         \
	{                                                                                                                                  \
		Key * parentKey = keyNew (PARENT_KEY, KEY_END);                                                                            \
		bool success = 1;
#define TEST_END                                                                                                                           \
	success = 1;                                                                                                                       \
	keyDel (parentKey);                                                                                                                \
	}

#define TEST_CHECK(expression, message)                                                                                                    \
	success = (expression);                                                                                                            \
	succeed_if (success, message);

ELEKTRA_UNUSED static void printAllKeysAndCorrespondingMetaKeys (KeySet * ks)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		printf ("keyName: %s, keyValue: %s\n", keyName (current), keyString (current));

		printf ("-----------------\n");

		KeySet * currentMetaKeys = keyMeta (current);
		for (elektraCursor it2 = 0; it2 < ksGetSize (currentMetaKeys); it2++)
		{
			Key * currentMetKey = ksAtCursor (currentMetaKeys, it2);
			printf ("metaKeyName: %s, metaKeyValue: %s\n", keyName (currentMetKey), keyString (currentMetKey));
		}

		printf ("-----------------\n");
	}
}

/**
 * Output an info if it exists.
 *
 * @param parentKey the parent key to extract the info from
 * @return 0 - if no output was available
 */
static int output_info (Key * parentKey)
{
	if (!parentKey)
	{
		return 0;
	}

	char * metaName = elektraFormat ("%s/%s", INFO_KEY, "description");
	const Key * infoDescription = keyGetMeta (parentKey, metaName);
	elektraFree (metaName);

	printf ("info with description on key %s: %s\n", keyName (parentKey), keyString (infoDescription));

	return 1;
}

/**
 * This test should verify that there is an error when a key is required by the specification
 * but is not present in the configuration.
 *
 * Sample:
 * 	spec:/sw/org/a => meta:/require = true
 *	user:/sw/org/b
 *
 * Should fail with error as the key is missing in the configuration but is required.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_require_meta_key_and_missing_key_should_error (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "require", "true", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/b", KEY_VALUE, "19", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_error (parentKey) == 0, "no errors found");
		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_ERROR, "plugin should have exited with error status");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that a default key is created in the `default` namespace when it is
 * missing in the configuration but the specification provides a meta key default (meta:/default).
 *
 * Sample:
 * 	spec:/sw/org/a	=> meta:/default = 17
 * 	no configuration
 *
 * Should create default:/sw/org/a with default value 17.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_default_meta_key_and_missing_key_should_create_key_with_default (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_DEFAULT)
			{
				if (elektraStrCmp (keyBaseName (current), "a") == 0)
				{
					succeed_if_same_string (keyString (current), "17");
					succeed_if (keyGetMeta (current, "default") != 0, "default meta key should exist");
				}
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that meta keys are only copied to the correct configuration which
 * matches the specification.
 *
 * Sample:
 * 	spec:/sw/org/a => meta:/default = 17
 * 	user:/sw/org/a => matches and will be copied to this key
 * 	user:/sw/org/b => does not match and will not be copied
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_only_to_keys_specified_in_specification_should_succeeded (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/a", KEY_VALUE, "17", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/b", KEY_VALUE, "18", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				if (elektraStrCmp (keyBaseName (current), "a") == 0)
				{
					succeed_if (keyGetMeta (current, "default") != 0, "key a needs default meta key");
				}
				else if (elektraStrCmp (keyBaseName (current), "b") == 0)
				{
					succeed_if (keyGetMeta (current, "default") == 0, "key b should not have default");
				}
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded")

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that if a key was defined in the specification, has no default meta key and is not in the
 * configuration (no other namespace), then it should show an info.
 *
 * Sample:
 * 	spec:/sw/org/a => meta:/somemetakey = hello
 *
 * No key created. Info shown.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_missing_key_and_no_default_should_info (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "somemetakey", "hello", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_info (parentKey), "no infos available");
		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that if a parent key with the namespace is passed, key was not found and default meta key
 * is set, the namespace does not get prepended on adding the default key to the default namespace.
 *
 * Sample:
 * 	spec:/sw/org/a => meta:/default = 17
 * 	PARENT_KEY: user:/sw/org/a
 *
 * 	default key should be created in default namespace => key: default:/sw/org/a, value: 17 => meta:/default = 17
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_parent_key_containing_namespace_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END));

		Key * parentKeyWithNamespace = keyNew (PARENT_KEY_WITH_NAMESPACE, KEY_END);
		int result = elektraSpecCopy (NULL, ks, parentKeyWithNamespace, isKdbGet);

		const char * keyNameToMatch = elektraFormat ("default:%s/%s", PARENT_KEY, "a");
		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_DEFAULT)
			{
				succeed_if_same_string (keyName (current), keyNameToMatch)
			}
		}

		elektraFree ((char *) keyNameToMatch);
		keyDel (parentKeyWithNamespace);
		ksDel (ks);

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
	}
	TEST_END
}

/**
 * This test should verify that if wildcard specification exists (with one underline) it should copy all meta data to
 * the existing configuration.
 *
 * Sample:
 * 	spec:/sw/org/a/_/name => meta:/description = "This is a name"
 * 	user:/sw/org/a/server/name => value = mailserver1
 *
 * Should copy meta:/description = "This is a name" to user:/sw/org/a/server/name.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_wildcard_specification_only_one_underline_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		const char * descriptionToMatch = "This is a name";
		const char * userNamespace = "user:";
		const char * keyNameToMatch = elektraFormat ("%s/server/name", PARENT_KEY);

		const char * formattedKeyName = elektraFormat ("%s/%s/%s", userNamespace, PARENT_KEY, keyNameToMatch);

		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "_/name", KEY_META, "description", descriptionToMatch, KEY_END));
		ksAppendKey (ks, keyNew (formattedKeyName, KEY_VALUE, "mailserver1", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				if (elektraStrCmp (strchr (keyName (current), '/'), keyNameToMatch) == 0)
				{
					const Key * descriptionMetaKey = keyGetMeta (current, "description");
					succeed_if_same_string (keyString (descriptionMetaKey), descriptionToMatch);
				}
			}
		}

		elektraFree ((char *) formattedKeyName);
		elektraFree ((char *) keyNameToMatch);
		ksDel (ks);

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
	}
	TEST_END
}

/**
 * This test should verify that if a wildcard specification exists, a key is required, but does not exist, then it should fail.
 *
 * Sample:
 * 	spec:/sw/org/a/_/name => meta:/require = true
 *
 * No configuration => should fail
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_wildcard_specification_and_required_no_match_should_fail (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "_/name", KEY_META, "require", "true", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_error (parentKey) == 0, "no errors found");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_ERROR, "plugin should have failed");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test verifies that the key in the configuration matches a spec key with two wildcards.
 *
 * Sample:
 * 	spec:/org/sw/_/server/_/name => meta:/description = place
 *
 * 	user:/org/sw/test/server/test2/name
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_wildcard_two_underlines_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		const char * namespace = "user:/";
		const char * keyname = elektraFormat ("%s/test/server/test2/name", PARENT_KEY);

		const char * formattedKeyName = elektraFormat ("%s%s", namespace, keyname);

		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/_/server/_/name", KEY_META, "description", "place", KEY_END));
		ksAppendKey (ks, keyNew (formattedKeyName, KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				if (elektraStrCmp (keyname, strchr (keyName (current), '/')) == 0)
				{
					const Key * key = keyGetMeta (current, "description");
					succeed_if_same_string (keyString (key), "place");
				}
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		elektraFree ((char *) formattedKeyName);
		elektraFree ((char *) keyname);
		ksDel (ks);
	}
	TEST_END
}

/**
 * This test verifies that the trailing underline (wildcard) matches the correct configuration.
 *
 * Sample:
 * 	spec:/sw/org/_/server/_ => meta:/description = place
 *
 * 	user:/sw/org/test/server/test1
 * 	user:/sw/org/test/server/test2
 * 	user:/sw/org/test/server/test3
 * 	user:/sw/org/test/server2/test4	=> should not get the meta key description
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_wildcard_with_trailing_underline (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/_/server/_", KEY_META, "description", "place", KEY_END));
		ksAppendKey (ks, keyNew ("user:/sw/org/test/server/test1", KEY_END));
		ksAppendKey (ks, keyNew ("user:/sw/org/test/server/test2", KEY_END));
		ksAppendKey (ks, keyNew ("user:/sw/org/test/server/test3", KEY_END));
		ksAppendKey (ks, keyNew ("user:/sw/org/test/server2/test4", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				const char * baseName = keyBaseName (current);
				if (elektraStrCmp (baseName, "test1") == 0 || elektraStrCmp (baseName, "test2") == 0 ||
				    elektraStrCmp (baseName, "test3") == 0)
				{
					const Key * key = keyGetMeta (current, "description");
					succeed_if_same_string (keyString (key), "place");
				}

				if (elektraStrCmp (baseName, "test4") == 0)
				{
					succeed_if (keyGetMeta (current, "description") == 0, "test4 should not have metadata");
				}
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test verifies that the metadata with just a wildcard matches all keys at the first level.
 *
 * Sample:
 * 	spec:/sw/org/_ => meta:/description = place
 *
 * 	user:/sw/org/test1
 * 	user:/sw/org/test2
 * 	user:/sw/org/test3/test4
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_just_wildcard (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		const char * formattedKeyTest1 = elektraFormat ("user:%s/test1", PARENT_KEY);
		const char * formattedKeyTest2 = elektraFormat ("user:%s/test2", PARENT_KEY);
		const char * formattedKeyTest3_4 = elektraFormat ("user:%s/test3/test4", PARENT_KEY);

		KeySet * ks = ksNew (0, KS_END);
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/_", KEY_META, "description", "place", KEY_END));
		ksAppendKey (ks, keyNew (formattedKeyTest1, KEY_END));
		ksAppendKey (ks, keyNew (formattedKeyTest2, KEY_END));
		ksAppendKey (ks, keyNew (formattedKeyTest3_4, KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				if (elektraStrCmp (keyBaseName (current), "test4") != 0)
				{
					const Key * metaKey = keyGetMeta (current, "description");
					succeed_if_same_string (keyString (metaKey), "place");
				}
			}
		}

		Key * test3Key = ksLookupByName (ks, "/sw/org/test3/test4", 0);
		const Key * metaKeyTest3 = keyGetMeta (test3Key, "description");
		succeed_if (metaKeyTest3 == 0, "meta key should not be found");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		elektraFree ((char *) formattedKeyTest1);
		elektraFree ((char *) formattedKeyTest2);
		elektraFree ((char *) formattedKeyTest3_4);

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that when a wildcard specification and array specification exist and they collide in semantics
 * an error is appended to the parent key.
 *
 * Sample:
 * 	spec:/sw/org/server/_/name => meta:/description = value1
 * 	spec:/sw/org/server/#/name => meta:/description = value2
 *
 * This is a collision here. .../server/ is specified as array element but also matched with another wildcard specification.
 * The spec plugin can in this case not know what specification is correct and therefore appends an error.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_wildcard_array_specification_collision_should_fail (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server/_/name", KEY_META, "description", "value1", KEY_END));
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "description", "value2", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_error (parentKey) == 0, "no errors found");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_ERROR, "plugin should have failed");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that a specification key with array element is instantiated correctly and validated correctly.
 *
 * Sample:
 * 	spec:/sw/org/server/#/name => meta:/description = The name of the server
 *
 * 	/sw/org/server => meta:/array = 4
 *
 * 	user:/sw/org/server/#0/name
 * 	user:/sw/org/server/#1/name
 * 	user:/sw/org/server/#2/name
 * 	user:/sw/org/server/#3/name
 *
 * All four configurations should have the description meta key.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_array_specification_should_copy_to_correct_configuration (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		char * metaKeyDescriptionValue = "The name of the server";
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "description", metaKeyDescriptionValue, KEY_END));
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server", KEY_META, "array", "4", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/server/#0/name", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/server/#1/name", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/server/#2/name", KEY_END));
		ksAppendKey (ks, keyNew ("user:/" PARENT_KEY "/server/#3/name", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_USER)
			{
				const Key * metaKeyDescription = keyGetMeta (current, "description");
				succeed_if_same_string (keyString (metaKeyDescription), metaKeyDescriptionValue);
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify that if an array specification exists and an array size as well, but no default value it
 * does not instantiate the array elements.
 *
 * Sample:
 * 	spec:/sw/org/server/#/name => meta:/description = "The name of the server"
 * 	spec:/sw/org/server => meta:/array = 4
 *
 * No configuration. It should also not instantiate any keys.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_array_specification_without_default_meta_key_should_not_instantiate_any_key (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		char * metaKeyDescriptionValue = "The name of the server";
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_VALUE, "testserver", KEY_META, "description",
					 metaKeyDescriptionValue, KEY_END));
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server", KEY_META, "array", "4", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			succeed_if (keyGetNamespace (ksAtCursor (ks, it)) != KEY_NS_DEFAULT, "should not contain default");
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * This test should verify if an array specification exists and has default meta key and no configuration for this specification key exists
 * it creates all the necessary array keys.
 *
 * Sample:
 * 	spec:/sw/org/server/#/name => meta:/default = mail, meta:/description = The name of the server
 * 	spec:/sw/org/server => meta:/array = 4
 *
 * No configuration. Should instantiate array keys from #0-#3 with all meta keys copied to each of these elements.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_array_specification_with_default_meta_key_should_instantiate (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		char * metaKeyDescriptionValue = "The name of the server";
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "default", "mail", KEY_META, "description",
					 metaKeyDescriptionValue, KEY_END));
		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/server", KEY_META, "array", "4", KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		int count = 0;
		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);

			elektraNamespace namespace = keyGetNamespace (current);
			if (namespace != KEY_NS_SPEC)
			{
				if (namespace == KEY_NS_DEFAULT)
				{
					count++;
				}
				succeed_if (keyGetNamespace (current) == KEY_NS_DEFAULT,
					    "should not contain other namespaces than default and spec");
			}
		}

		succeed_if (count == 4, "not enough array keys created");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * Test should verify that if meta keys get copied with elektraSpecCopy, they should get removed by elektraSpecRemove.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_remove_spec_keys_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "default value a", KEY_META, "test1", "value1",
					 KEY_META, "test2", "description2", KEY_END));

		int resultCopy = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		// check if meta keys were copied
		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_DEFAULT)
			{
				KeySet * metaKeysForCurrent = keyMeta (current);
				succeed_if (ksGetSize (metaKeysForCurrent) == 3, "there should be three meta keys");
			}
		}

		int resultRemove = elektraSpecRemove (NULL, ks, parentKey);

		// check if meta keys were removed
		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_DEFAULT)
			{
				KeySet * metaKeysForCurrent = keyMeta (current);
				succeed_if (ksGetSize (metaKeysForCurrent) == 0, "meta keys should be empty");
			}
		}

		TEST_CHECK (resultCopy == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
		TEST_CHECK (resultRemove == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * Test should verify that a specification key with an array element `#` at the end of the key name with default meta key is recognized
 * correctly and default array key name is instantiated.
 *
 * Sample:
 * 	spec:/sw/org/server/a/# => meta:/default = default value a
 *
 * No configuration. Should instantiate a default array key.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_array_specification_as_last_element_should_create_array_default (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		const char * metaKeyDefaultValue = "default value a";
		KeySet * ks = ksNew (0, KS_END);

		ksAppendKey (ks, keyNew ("spec:/" PARENT_KEY "/a/#", KEY_META, "default", metaKeyDefaultValue, KEY_END));

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			Key * current = ksAtCursor (ks, it);
			if (keyGetNamespace (current) == KEY_NS_DEFAULT)
			{
				succeed_if_same_string (keyString (current), metaKeyDefaultValue);
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");

		ksDel (ks);
	}
	TEST_END
}

/**
 * Test should verify the menu test example from check_external_example_codegen_menu test.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_example_menu_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	Key * parentKey = keyNew ("/sw/example/menu/#0/current", KEY_END);

	KeySet * ks = ksNew (0, KS_END);

	Key * specKeyCommand = keyNew ("spec:/sw/example/menu/#0/current/menu/#/command", KEY_END);
	keySetMeta (specKeyCommand, "meta:/default", "");
	keySetMeta (specKeyCommand, "meta:/type", "string");
	ksAppendKey (ks, specKeyCommand);

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu", KEY_META, "array", "#1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#0/name", KEY_VALUE, "Main Menu", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#1/name", KEY_VALUE, "Menu 1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#1/command", KEY_VALUE, "the executed command", KEY_END));

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	Key * menuCommand = ksLookupByName (ks, "user:/sw/example/menu/#0/current/menu/#1/command", 0);
	succeed_if (menuCommand != NULL, "should have menu 1 command");
	KeySet * menuCommandMeta = keyMeta (menuCommand);
	succeed_if (menuCommandMeta != NULL, "should have menu 1 command meta");
	succeed_if_fmt (ksGetSize (menuCommandMeta) == 2, "menu 1 command should have 2 meta keys, was %zu", ksGetSize (menuCommandMeta));
	succeed_if (ksLookupByName (menuCommandMeta, "meta:/type", 0), "menu 1 command meta should have key meta:/type");
	succeed_if (ksLookupByName (menuCommandMeta, "meta:/default", 0), "menu 1 command meta should have key meta:/default");

	keyDel (specKeyCommand);
	keyDel (parentKey);
	ksDel (ks);
}

/**
 * Test should verify that default for key name is created in default namespace in case it has array element in parent key.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_example_highlevel_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	Key * parentKey = keyNew ("/sw/example/highlevel/#0/current", KEY_END);

	KeySet * ks = ksNew (0, KS_END);

	Key * printKey = keyNew ("spec:/sw/example/highlevel/#0/current/print", KEY_END);
	keySetMeta (printKey, "meta:/default", "0");
	keySetMeta (printKey, "meta:/type", "boolean");

	ksAppendKey (ks, printKey);

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	Key * highLevelPrint = ksLookupByName (ks, "default:/sw/example/highlevel/#0/current/print", 0);
	succeed_if (highLevelPrint != NULL, "should have default key print");
	KeySet * highLevelPrintMetaKeys = keyMeta (highLevelPrint);
	succeed_if (highLevelPrintMetaKeys != NULL, "print key should have meta keys");
	succeed_if_fmt (ksGetSize (highLevelPrintMetaKeys) == 2, "print key should have 2 meta keys, was %zu",
			ksGetSize (highLevelPrintMetaKeys));
	succeed_if (ksLookupByName (highLevelPrintMetaKeys, "meta:/type", 0), "print key should have key meta:/type");
	succeed_if (ksLookupByName (highLevelPrintMetaKeys, "meta:/default", 0), "print key should have key meta:/default");

	keyDel (printKey);
	keyDel (parentKey);
	ksDel (ks);
}

/**
 * Test should verify that if a array specification key with array size exists, the correct number of array key elements is created.
 *
 * Sample:
 * 	spec:/sw/example/menu/#0/current/menu
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_example_menu_with_array_size_including_array_element_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	Key * parentKey = keyNew ("/sw/example/menu/#0/current", KEY_END);

	KeySet * ks = ksNew (0, KS_END);

	Key * specCommandArraySize = keyNew ("spec:/sw/example/menu/#0/current/menu", KEY_END);
	Key * specKeyCommand = keyNew ("spec:/sw/example/menu/#0/current/menu/#/command", KEY_END);

	keySetMeta (specCommandArraySize, "meta:/array", "#4");
	keySetMeta (specKeyCommand, "meta:/default", "test");

	ksAppendKey (ks, specCommandArraySize);
	ksAppendKey (ks, specKeyCommand);

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	int size = 0;
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * key = ksAtCursor (ks, it);
		if (keyGetNamespace (key) == KEY_NS_DEFAULT)
		{
			size++;
			KeySet * metaKeys = keyMeta (key);

			succeed_if (ksGetSize (metaKeys) == 1, "should not have more than 1 meta key");
			succeed_if_same_string (keyString (keyGetMeta (key, "meta:/default")), "test");
		}
	}

	succeed_if (size == 5, "array size should equal 4");

	keyDel (specCommandArraySize);
	keyDel (specKeyCommand);
	keyDel (parentKey);
	ksDel (ks);
}

/**
 * Test should verify that a key with an array element in parent key gets meta data copied correctly.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_normal_key_with_meta_data_and_array_element_in_key_name_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	Key * parentKey = keyNew ("/sw/example/highlevel/#0/current", KEY_END);

	KeySet * ks = ksNew (0, KS_END);

	Key * specString = keyNew ("spec:/sw/example/highlevel/#0/current/myfloatarray/#", KEY_END);
	keySetMeta (specString, "meta:/default", "2.5");
	keySetMeta (specString, "meta:/type", "string");

	ksAppendKey (ks, specString);

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	keyDel (parentKey);
	keyDel (specString);
	ksDel (ks);
}

/**
 * Test should verify that the example_codegen_menu.sh test works.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void example_codegen_menu_test_should_succeed (bool isKdbGet)
{

	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	Key * parentKey = keyNew ("/sw/example/menu/#0/current", KEY_END);

	KeySet * ks = ksNew (0, KS_END);

	Key * specKeyCommand = keyNew ("spec:/sw/example/menu/#0/current/menu/#/command", KEY_END);
	keySetMeta (specKeyCommand, "meta:/default", "");
	keySetMeta (specKeyCommand, "meta:/type", "string");
	ksAppendKey (ks, specKeyCommand);


	Key * arraySpec = keyNew ("user:/sw/example/menu/#0/current/menu", KEY_END);
	keySetMeta (arraySpec, "meta:/array", "#4");
	ksAppendKey (ks, arraySpec);

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#0/name", KEY_VALUE, "Main Menu", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#1/name", KEY_VALUE, "Menu 1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#2/name", KEY_VALUE, "Menu 2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#3/name", KEY_VALUE, "Menu 2.1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#4/name", KEY_VALUE, "Menu 2.2", KEY_END));

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#1/command", KEY_VALUE, "echo \"Hello from Menu 1\"", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#2/command", KEY_VALUE, "echo \"Hello from Menu 2\"", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#3/command", KEY_VALUE, "echo \"Hello from Menu 2.1\"", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#4/command", KEY_VALUE, "echo \"Hello from Menu 2.2\"", KEY_END));

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#0/children", KEY_META, "array", "#1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#0/children/#0", KEY_VALUE, "@/menu/#1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#0/children/#1", KEY_VALUE, "@/menu/#2", KEY_END));

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#2/children", KEY_META, "array", "#1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#2/children/#0", KEY_VALUE, "@/menu/#3", KEY_END));
	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/menu/#2/children/#1", KEY_VALUE, "@/menu/#4", KEY_END));

	ksAppendKey (ks, keyNew ("user:/sw/example/menu/#0/current/main", KEY_VALUE, "@/menu/#0", KEY_END));

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	Key * commandToLookup = keyNew ("/sw/example/menu/#0/current/menu/#0/command", KEY_END);
	Key * lookupCommand = ksLookup (ks, commandToLookup, 0);

	keyDel (commandToLookup);
	succeed_if (lookupCommand != 0, "should have found #0 command");

	keyDel (parentKey);
	keyDel (specKeyCommand);
	keyDel (arraySpec);
	ksDel (ks);
}

/**
 * Test should verify that an array specification with an array size of 1 gets only 1 array instantiated and the default value assigned.
 *
 * Sample:
 * 	user:/tests/script/gen/highlevel/externalspec/myfloatarray
 * 		array => #0
 *	spec:/tests/script/gen/highlevel/externalspec/myfloatarray/#
 *		default => 2.5
 *		type => float
 *
 * No other configuration.
 * It should create /tests/script/gen/highlevel/externalspec/myfloatarray/#0 in default namespace.
 * Value should be 2.5.
 * Array should only have size 1.
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_with_array_specification_with_array_size_one_should_succeed (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	KeySet * ks = ksNew (0, KS_END);

	Key * parentKey = keyNew ("/tests/script/gen/highlevel/externalspec", KEY_END);

	Key * arrayKeySize = keyNew ("user:/tests/script/gen/highlevel/externalspec/myfloatarray", KEY_END);
	keySetMeta (arrayKeySize, "array", "#0");

	Key * specArrayKey = keyNew ("spec:/tests/script/gen/highlevel/externalspec/myfloatarray/#", KEY_END);
	keySetMeta (specArrayKey, "default", "2.5");
	keySetMeta (specArrayKey, "type", "float");

	ksAppendKey (ks, specArrayKey);
	ksAppendKey (ks, parentKey);
	ksAppendKey (ks, arrayKeySize);

	elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	int count = 0;
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * key = ksAtCursor (ks, it);
		if (keyGetNamespace (key) == KEY_NS_DEFAULT)
		{
			count++;

			const char * value = keyString (key);
			succeed_if_same_string (value, "2.5");
		}
	}

	succeed_if (count == 1, "myfloatarray should have size 1");

	keyDel (parentKey);
	keyDel (specArrayKey);
	keyDel (arrayKeySize);
	ksDel (ks);
}

int main (void)
{
	test_hook_copy_with_require_meta_key_and_missing_key_should_error (false);
	test_hook_copy_with_default_meta_key_and_missing_key_should_create_key_with_default (false);
	test_hook_copy_only_to_keys_specified_in_specification_should_succeeded (false);
	test_hook_copy_with_missing_key_and_no_default_should_info (false);
	test_hook_copy_with_parent_key_containing_namespace_should_succeed (false);

	test_hook_copy_with_wildcard_specification_only_one_underline_should_succeed (false);
	test_hook_copy_with_wildcard_specification_and_required_no_match_should_fail (false);
	test_hook_copy_with_wildcard_two_underlines_should_succeed (false);
	test_hook_copy_with_wildcard_with_trailing_underline (false);
	test_hook_copy_with_just_wildcard (false);
	test_hook_copy_with_wildcard_array_specification_collision_should_fail (false);

	test_hook_copy_with_array_specification_should_copy_to_correct_configuration (false);
	test_hook_copy_with_array_specification_without_default_meta_key_should_not_instantiate_any_key (false);
	test_hook_copy_with_array_specification_with_default_meta_key_should_instantiate (false);

	test_hook_remove_spec_keys_should_succeed (true);

	test_hook_copy_with_array_specification_as_last_element_should_create_array_default (false);

	test_example_menu_should_succeed (true);
	test_example_highlevel_should_succeed (true);
	test_example_menu_with_array_size_including_array_element_should_succeed (true);

	test_normal_key_with_meta_data_and_array_element_in_key_name_should_succeed (true);
	example_codegen_menu_test_should_succeed (true);
	test_with_array_specification_with_array_size_one_should_succeed (true);

	return 0;
}
