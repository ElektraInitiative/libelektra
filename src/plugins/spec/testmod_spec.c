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
 *
 *
 * @param isKdbGet boolean value indicating if it is a kdb get call
 */
static void test_hook_copy_only_to_keys_specified_in_specification (bool isKdbGet)
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

static void test_hook_copy_with_parent_key_containing_namespace (bool isKdbGet)
{
	printf ("test %s, isKdbGet=%d\n", __func__, isKdbGet);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY_WITH_NAMESPACE "/a", KEY_META, "default", "17", KEY_END), KS_END);

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

int main (void)
{
	test_hook_copy_with_require_meta_key_and_missing_key_should_error (false);
	test_hook_copy_with_default_meta_key_and_missing_key_should_create_key_with_default (false);
	test_hook_copy_only_to_keys_specified_in_specification (false);
	test_hook_copy_with_missing_key_and_no_default_should_info (false);
	test_hook_copy_with_parent_key_containing_namespace (false);

	return 0;
}
