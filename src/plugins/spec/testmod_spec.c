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
		KeySet * conf = ksDup (_conf);                                                                                             \
		PLUGIN_OPEN ("spec");                                                                                                      \
		Key * parentKey = keyNew (PARENT_KEY, KEY_END);                                                                            \
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

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);
	TEST_BEGIN
	{
		KeySet * ks =
			ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "default", "17", KEY_META, "othermeta", "", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

		succeed_if (lookup != NULL, ".../a not found");
		succeed_if_same_string (keyString (lookup), "17");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "default", "17", KEY_META, "othermeta", "", KEY_END),
				     keyNew ("user" PARENT_KEY "/a", KEY_VALUE, "19", KEY_META, "default", "19", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should fail");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_assign_condition (void)
{
	printf ("test assign/condition\n");

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks =
			ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "assign/condition", "17", KEY_META, "othermeta", "", KEY_END),
			       KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);

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

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/_", KEY_META, "default", "17", KEY_META, "othermeta", "", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/x", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a/x", 0);

		succeed_if (lookup != NULL, ".../a/x not found");
		// succeed_if_same_string (keyString (lookup), "17"); // TODO: wildcard defaults, not possible right now
		succeed_if_same_string (keyString (keyGetMeta (lookup, "default")), "17");
		succeed_if (keyGetMeta (lookup, "othermeta") != NULL, "metadata missing");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/_", KEY_META, "require/count", "2", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/x", KEY_END), keyNew ("user" PARENT_KEY "/a/y", KEY_END), KS_END);

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

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "require", "", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "require", "", KEY_END),
				     keyNew ("user" PARENT_KEY "/a", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_array (void)
{
	printf ("test array\n");

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#", KEY_META, "default", "7", KEY_META, "type", "long", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#5", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
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
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "default", "7", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#2", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
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
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "default", "7", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#2", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
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
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "default", "7", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#3", KEY_END),
				     keyNew ("user" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#2", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
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
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "default", "7", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array/min", "#3", KEY_END),
				     keyNew ("user" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#2", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "default", "7", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#3", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#2", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/#0/b/c", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#", KEY_META, "default", "", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/x", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END
	ksDel (_conf);
}

static void test_require_array (void)
{
	printf ("test require array\n");

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "require", "", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#0", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet shouldn't succeed");

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "require", "", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#0", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/#0/b/#0", KEY_END), KS_END);

		TEST_CHECK (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
		TEST_ON_FAIL (output_error (parentKey));

		ksDel (ks);
	}
	TEST_END

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a/#/b/#", KEY_META, "require", "", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a", KEY_META, "array", "#0", KEY_END),
				     keyNew ("spec" PARENT_KEY "/a/#/b", KEY_META, "array", "#1", KEY_END),
				     keyNew ("user" PARENT_KEY "/a/#0/b/#0", KEY_END), KS_END);

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

	KeySet * _conf = ksNew (1, keyNew ("user/conflict/get", KEY_VALUE, "ERROR", KEY_END), KS_END);

	TEST_BEGIN
	{
		KeySet * ks = ksNew (10, keyNew ("spec" PARENT_KEY "/a", KEY_META, "othermeta", "", KEY_END),
				     keyNew ("user" PARENT_KEY "/a", KEY_END), KS_END);

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
	test_array ();
	test_require_array ();
	// test_remove_meta ();

	print_result ("testmod_spec");

	return nbError;
}
