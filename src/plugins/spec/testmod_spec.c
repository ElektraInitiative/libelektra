#include "spec-new.h"

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
	{                                               										   \
		Key * parentKey = keyNew (PARENT_KEY, KEY_END);                                                                            \
		bool success = 1;
#define TEST_END                                                                                                                      	   \
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

	const Key * infoDescription = keyGetMeta (parentKey, elektraFormat ("%s/%s", INFO_KEY, "description"));
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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "require", "true", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/b", KEY_VALUE, "19", KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END), KS_END);

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
 * 	user:/sw/org/a => matches and will copied to this key
 * 	user:/sw/org/b => does not match and will not be copied
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_only_to_keys_specified_in_specification_should_succeeded (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", KEY_VALUE, "17", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/b", KEY_VALUE, "18", KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "somemetakey", "hello", KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "default", "17", KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "_/name", KEY_META, "description", descriptionToMatch,
						 KEY_END), keyNew (elektraFormat ("%s/%s/%s", userNamespace, PARENT_KEY, keyNameToMatch),
					     KEY_VALUE, "mailserver1", KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "_/name", KEY_META, "require", "true", KEY_END), KS_END);

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_error (parentKey) == 0, "no errors found");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_ERROR, "plugin should have failed");
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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/_/server/_/name", KEY_META, "description", "place",
							 KEY_END), keyNew (elektraFormat ("%s%s", namespace, keyname),
					     KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/_/server/_", KEY_META, "description", "place",
						 KEY_END),
				     keyNew ("user:/sw/org/test/server/test1", KEY_END),
				     keyNew ("user:/sw/org/test/server/test2", KEY_END),
				     keyNew ("user:/sw/org/test/server/test3", KEY_END),
				     keyNew ("user:/sw/org/test/server2/test4", KEY_END), KS_END);

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
					succeed_if (keyGetMeta (current, "description") == 0,
						    "test4 should not have metadata");
				}
			}
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/_", KEY_META, "description", "place", KEY_END),
				     keyNew (elektraFormat ("user:%s/test1", PARENT_KEY), KEY_END),
				     keyNew (elektraFormat ("user:%s/test2", PARENT_KEY), KEY_END),
				     keyNew (elektraFormat ("user:%s/test3/test4", PARENT_KEY), KEY_END), KS_END);

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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/server/_/name", KEY_META, "description", "value1",
						 KEY_END), keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "description",
					     "value2", KEY_END), KS_END);

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		succeed_if (output_error (parentKey) == 0, "no errors found");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_ERROR, "plugin should have failed");
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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "description",
						 metaKeyDescriptionValue, KEY_END), keyNew ("spec:/" PARENT_KEY "/server",
					     KEY_META, "array", "4", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/server/#0/name", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/server/#1/name", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/server/#2/name", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/server/#3/name", KEY_END), KS_END);

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
	}
	TEST_END
}

/**
 * This test should verify that if a array specification exists and an array size as well, but no default value it
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
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_VALUE, "testserver",
						 KEY_META, "description", metaKeyDescriptionValue, KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/server", KEY_META, "array", "4", KEY_END), KS_END);

		int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

		for (elektraCursor it = 0; it < ksGetSize (ks); it++)
		{
			succeed_if (keyGetNamespace (ksAtCursor (ks, it)) != KEY_NS_DEFAULT, "should not contain default");
		}

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
	}
	TEST_END
}

/**
 * This test should verify if a array specification exists and has default meta key and no configuration for this specification key exists
 * it creates
 *
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_with_array_specification_with_default_meta_key_should_instantiate (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		char * metaKeyDescriptionValue = "The name of the server";
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/server/#/name", KEY_META, "default", "mail", KEY_META, "description",
						 metaKeyDescriptionValue, KEY_END), keyNew ("spec:/" PARENT_KEY "/server", KEY_META,
					     "array", "4", KEY_END), KS_END);

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
				succeed_if (keyGetNamespace (current) == KEY_NS_DEFAULT, "should not contain default");
			}
		}

		succeed_if (count == 4, "not enough array keys created");

		TEST_CHECK (result == ELEKTRA_PLUGIN_STATUS_SUCCESS, "plugin should have succeeded");
	}
	TEST_END
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

	return 0;
}
