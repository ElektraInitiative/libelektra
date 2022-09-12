/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include "spec.h"

#include <tests_plugin.h>

#define PARENT_KEY "/tests/spec"

#define TEST_BEGIN                                                                                                                         \
	{                                                                                                                                  \
		ElektraKeyset * conf = ksDup (_conf);                                                                                             \
		PLUGIN_OPEN ("spec");                                                                                                      \
		ElektraKey * parentKey = keyNew (PARENT_KEY, ELEKTRA_KEY_END);                                                                            \
		bool success = 1;

#define TEST_END                                                                                                                           \
	success = 1;                                                                                                                       \
	keyDel (parentKey);                                                                                                                \
	PLUGIN_CLOSE ();                                                                                                                   \
	}

#define TEST_CHECK(expression, message)                                                                                                    \
	success = (expression);                                                                                                            \
	succeed_if (success, message);

#define TEST_ON_FAIL(block)                                                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (!success)                                                                                                              \
		{                                                                                                                          \
			block;                                                                                                             \
		}                                                                                                                          \
	} while (0)

static void test_default (void)
{
	printf ("test default\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "default", "17", ELEKTRA_KEY_META, "othermeta", "", ELEKTRA_KEY_END),
				     ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (lookup), "17");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "default", "17", ELEKTRA_KEY_META, "othermeta", "", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_VALUE, "19", ELEKTRA_KEY_META, "default", "19", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should fail");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_assign_condition (void)
{
	printf ("test assign/condition\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (
			10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "assign/condition", "17", ELEKTRA_KEY_META, "othermeta", "", ELEKTRA_KEY_END),
			ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (lookup), "");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "assign/condition")), "17");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_wildcard (void)
{
	printf ("test wildcard (_)\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/_", ELEKTRA_KEY_META, "default", "17", ELEKTRA_KEY_META, "othermeta", "", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/x", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a/x", 0);

		succeed_if (lookup != NULL, ".../a/x not found");
		// succeed_if_same_string (keyString (lookup), "17"); // TODO: wildcard defaults, not possible right now
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "17");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/_", ELEKTRA_KEY_META, "require/count", "2", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/x", ELEKTRA_KEY_END), keyNew ("user:/" PARENT_KEY "/a/y", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END

	ksDel (_conf);
}

static void test_require (void)
{
	printf ("test require\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_logMissing (void)
{
	printf ("test logMissing\n");

	ElektraKeyset * _conf = ksNew (2, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END),
				keyNew ("user:/missing/log", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		succeed_if (keyGetMeta (parentKey, "logs/spec/missing") != NULL, "missing key should have been logged");
		succeed_if_same_string (keyString (keyGetMeta (parentKey, "logs/spec/missing")), "#0");

		succeed_if (keyGetMeta (parentKey, "logs/spec/missing/#0") != NULL, "missing key should have been logged");
		succeed_if_same_string (keyString (keyGetMeta (parentKey, "logs/spec/missing/#0")), PARENT_KEY "/a");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		succeed_if (keyGetMeta (parentKey, "logs/spec/missing") == NULL, "missing key should not have been logged");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_array (void)
{
	printf ("test array\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_META, "type", "long", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#5");
		succeed_if (keyGetMeta (lookup, "type") == NULL, "parent shouldn't have copied metadata");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0", 0);
		succeed_if (lookup != NULL, ".../a/#0 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#1", 0);
		succeed_if (lookup != NULL, ".../a/#1 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#2", 0);
		succeed_if (lookup != NULL, ".../a/#2 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#3", 0);
		succeed_if (lookup != NULL, ".../a/#3 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#4", 0);
		succeed_if (lookup != NULL, ".../a/#4 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#5", 0);
		succeed_if (lookup != NULL, ".../a/#5 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "type")), "long");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#0");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b", 0);
		succeed_if (lookup != NULL, ".../a/#0/b not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#2");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#0", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#0 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#1", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#1 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#2", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#2 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#0");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b", 0);
		succeed_if (lookup != NULL, ".../a/#0/b not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#2");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#0", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#0 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#1", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#1 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#2", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#2 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#3", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#0");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b", 0);
		succeed_if (lookup != NULL, ".../a/#0/b not found");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "array")), "#2");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#0", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#0 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#1", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#1 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/#2", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/#2 not found");
		succeed_if_same_string (keyString (lookup), "7");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "7");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array/min", "#3", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "default", "7", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#3", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/#0/b/c", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_require_array (void)
{
	printf ("test require array\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/#0/b/#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b/#", ELEKTRA_KEY_META, "require", "", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "array", "#1", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/#0/b/#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_array_member (void)
{
	printf ("test array member\n");

	ElektraKeyset * _conf = ksNew (1, keyNew ("user:/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b", 0);
		succeed_if (lookup != NULL, ".../a/#0/b not found");
		succeed_if_same_string (keyString (lookup), "x");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "x");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#", ELEKTRA_KEY_META, "default", "", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/x", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("user:/" PARENT_KEY "/a/x", ELEKTRA_KEY_VALUE, "y", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b/c", ELEKTRA_KEY_META, "default", "y", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b/c", ELEKTRA_KEY_META, "default", "y", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/#0/b/c", ELEKTRA_KEY_VALUE, "z", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ElektraKey * lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b", 0);
		succeed_if (lookup != NULL, ".../a/#0/b not found");
		succeed_if_same_string (keyString (lookup), "x");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "x");

		lookup = ksLookupByName (ks, PARENT_KEY "/a/#0/b/c", 0);
		succeed_if (lookup != NULL, ".../a/#0/b/c not found");
		succeed_if_same_string (keyString (lookup), "z");
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "y");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b/c", ELEKTRA_KEY_META, "default", "y", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/x/b/c", ELEKTRA_KEY_VALUE, "z", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b", ELEKTRA_KEY_META, "default", "x", ELEKTRA_KEY_END),
				     keyNew ("spec:/" PARENT_KEY "/a/#/b/c", ELEKTRA_KEY_META, "default", "y", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/#0/b/c", ELEKTRA_KEY_VALUE, "z", ELEKTRA_KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a/x/b/c", ELEKTRA_KEY_VALUE, "z", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

/* TODO: find way to remove metadata safely after other plugins ran
static void test_remove_meta (void)
{
	printf ("test remove meta\n");

	KeySet * _conf = ksNew (1, keyNew ("user:/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "othermeta", "", KEY_END),
				     keyNew ("user:/" PARENT_KEY "/a", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

		succeed_if (lookup != NULL, ".../a not found");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		TEST_CHECK (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
		TEST_ON_FAIL (output_error (parentKey));

		lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

		succeed_if (lookup != NULL, ".../a not found");
		succeed_if (keyGetMeta (lookup, "othermeta") == NULL, "metadata not removed");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}
*/

int main (int argc, char ** argv)
{
	printf ("SPEC     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_default ();
	test_assign_condition ();
	test_wildcard ();
	test_require ();
	test_logMissing ();
	test_array ();
	test_require_array ();
	test_array_member ();
	// test_remove_meta ();

	print_result ("testmod_spec");

	return nbError;
}
