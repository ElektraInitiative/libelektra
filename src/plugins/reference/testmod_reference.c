/**
 * @file
 *
 * @brief Tests for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <elektra/kdbmacros.h>

#include <tests_plugin.h>

#include "reference.h"

#define BASE_KEY "user:/tests/reference"

#define xstr(a) str (a)
#define str(a) #a

#define SETUP(name)                                                                                                                        \
	printf ("test %s\n", #name);                                                                                                       \
                                                                                                                                           \
	Key * parentKey = keyNew (BASE_KEY, KEY_END);                                                                                      \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("reference");

#define TEARDOWN()                                                                                                                         \
	keyDel (parentKey);                                                                                                                \
	PLUGIN_CLOSE ();

#define RESET_ERRORS() keySetMeta (parentKey, "error", NULL);
#define RESET_WARNINGS() keySetMeta (parentKey, "warnings", NULL);

#define RESET(NAME)                                                                                                                        \
	ksDel (NAME##_keys);                                                                                                               \
	RESET_ERRORS ();                                                                                                                   \
	RESET_WARNINGS ();

#define WITH_KEYS(NAME, COUNT, ...)                                                                                                        \
	struct __test * NAME = NULL;                                                                                                       \
	KeySet * NAME##_keys = ksNew (COUNT, __VA_ARGS__, KS_END);

#define TEST_SET(NAME, EXPECTED)                                                                                                           \
	dummy (NAME);                                                                                                                      \
	succeed_if (plugin->kdbSet (plugin, NAME##_keys, parentKey) == EXPECTED, "** " #NAME ": unexpected set result");

#define EXPECT_ERROR(NAME, ERROR_CODE)                                                                                                     \
	dummy (NAME);                                                                                                                      \
	succeed_if (check_error0 (parentKey, ERROR_CODE), "** " #NAME ": got wrong error: " xstr (ERROR_CODE));

struct __test;

static inline void dummy (struct __test * ks ELEKTRA_UNUSED, ...)
{
}

static bool check_error0 (const Key * parentKey, const char * expectedError)
{
	const Key * errorKey = keyGetMeta (parentKey, "error/number");
	const char * actualError = errorKey != NULL ? keyString (errorKey) : NULL;
	return actualError != NULL && strcmp (actualError, expectedError) == 0;
}

static void test_single_negative (void)
{
	SETUP (single positive)

	WITH_KEYS (full, 2,
		   keyNew (BASE_KEY "/ref/full", KEY_VALUE, BASE_KEY "/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/target", KEY_END))
	TEST_SET (full, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (full)

	WITH_KEYS (relative1, 2,
		   keyNew (BASE_KEY "/ref/relative1", KEY_VALUE, "../../target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/target", KEY_END))
	TEST_SET (relative1, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative1)

	WITH_KEYS (relative2, 2,
		   keyNew (BASE_KEY "/ref/relative2", KEY_VALUE, "./target", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE,
			   KEY_END),
		   keyNew (BASE_KEY "/ref/relative2/target", KEY_END))
	TEST_SET (relative2, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative2)

	WITH_KEYS (relative3, 2,
		   keyNew (BASE_KEY "/ref/relative3", KEY_VALUE, "@/ref/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/ref/target", KEY_END))
	TEST_SET (relative3, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative3)

	WITH_KEYS (array, 5,
		   keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		   keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/target0", KEY_END),
		   keyNew (BASE_KEY "/target1", KEY_END))
	TEST_SET (array, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (array)

	TEARDOWN ()
}

static void test_single_positive (void)
{
	SETUP (single positive)

	WITH_KEYS (full_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/full", KEY_VALUE, BASE_KEY "/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/hidden/target", KEY_END))
	TEST_SET (full_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (full_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (full_negative)

	WITH_KEYS (relative1_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative1", KEY_VALUE, "../../target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/hidden/target", KEY_END))
	TEST_SET (relative1_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative1_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative1_negative)

	WITH_KEYS (relative2_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative2", KEY_VALUE, "./target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/hidden/ref/relative2/target", KEY_END))
	TEST_SET (relative2_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative2_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative2_negative)

	WITH_KEYS (relative3_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative3", KEY_VALUE, "@/ref/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/hidden/ref/target", KEY_END))
	TEST_SET (relative3_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative3_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative3_negative)

	WITH_KEYS (array_negative, 5,
		   keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		   keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		   keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/hidden/target0", KEY_END),
		   keyNew (BASE_KEY "/hidden/target1", KEY_END))
	TEST_SET (array_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (array_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (array_negative)

	TEARDOWN ();
}

static void test_recursive_positive (void)
{
	SETUP (recursive positive)

	WITH_KEYS (linked_list, 9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		   keyNew (BASE_KEY "/head/ref", KEY_VALUE, BASE_KEY "/element0", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		   keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		   keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		   keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		   keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		   keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		   keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/element3", KEY_END),
		   keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END))
	TEST_SET (linked_list, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (linked_list)

	WITH_KEYS (
		tree, 15, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		keyNew (BASE_KEY "/head/ref", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", KEY_VALUE, BASE_KEY "/element0", KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END), keyNew (BASE_KEY "/element0/ref", KEY_VALUE, "#2", KEY_END),
		keyNew (BASE_KEY "/element0/ref/#0", KEY_VALUE, BASE_KEY "/element00", KEY_END),
		keyNew (BASE_KEY "/element0/ref/#1", KEY_VALUE, BASE_KEY "/element01", KEY_END),
		keyNew (BASE_KEY "/element0/ref/#2", KEY_VALUE, BASE_KEY "/element02", KEY_END),
		keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		keyNew (BASE_KEY "/element00", KEY_VALUE, "element00", KEY_END),
		keyNew (BASE_KEY "/element01", KEY_VALUE, "element01", KEY_END),
		keyNew (BASE_KEY "/element02", KEY_VALUE, "element02", KEY_END),
		keyNew (BASE_KEY "/element02/ref", KEY_VALUE, BASE_KEY "/element020", KEY_END),
		keyNew (BASE_KEY "/element020", KEY_VALUE, "element020", KEY_END))
	TEST_SET (tree, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (tree)

	WITH_KEYS (
		merge, 9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		keyNew (BASE_KEY "/head/ref", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", KEY_VALUE, BASE_KEY "/element0", KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END))
	TEST_SET (merge, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (merge)

	WITH_KEYS (
		alternative, 16, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		keyNew (BASE_KEY "/head/ref", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", KEY_VALUE, BASE_KEY "/element0", KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END), keyNew (BASE_KEY "/element0/ref", KEY_VALUE, "#1", KEY_END),
		keyNew (BASE_KEY "/element0/ref/#0", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		keyNew (BASE_KEY "/element0/ref/#1", KEY_VALUE, BASE_KEY "/element4", KEY_END),
		keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		keyNew (BASE_KEY "/element1/altref", KEY_VALUE, BASE_KEY "/element2", KEY_META, CHECK_REFERENCE_KEYNAME,
			CHECK_REFERNCE_VALUE_ALTERNATIVE, KEY_END),
		keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		keyNew (BASE_KEY "/element2/altref", KEY_VALUE, BASE_KEY "/element3", KEY_END),
		keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END),
		keyNew (BASE_KEY "/element4", KEY_VALUE, "element4", KEY_END),
		keyNew (BASE_KEY "/element4/ref", KEY_VALUE, BASE_KEY "/element5", KEY_END),
		keyNew (BASE_KEY "/element5", KEY_VALUE, "element5", KEY_END))
	TEST_SET (alternative, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (alternative)

	TEARDOWN ()
}

static void test_recursive_negative (void)
{
	SETUP (recursive negative)

	WITH_KEYS (linked_list_negative, 9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		   keyNew (BASE_KEY "/head/ref", KEY_VALUE, BASE_KEY "/element0", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		   keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		   keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element4", KEY_END),
		   keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		   keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		   keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		   keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/element3", KEY_END),
		   keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END))
	TEST_SET (linked_list_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (linked_list_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	RESET (linked_list_negative)

	WITH_KEYS (linked_list_negative2, 9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		   keyNew (BASE_KEY "/head/ref", KEY_VALUE, BASE_KEY "/element0", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, KEY_END),
		   keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		   keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		   keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		   keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		   keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		   keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/head", KEY_END),
		   keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END))
	TEST_SET (linked_list_negative2, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (linked_list_negative2, ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	RESET (linked_list_negative2)

	TEARDOWN ()
}

static void test_restriction (void)
{
	SETUP (restriction positive)

	WITH_KEYS (positive, 2,
		   keyNew (BASE_KEY "/ref", KEY_VALUE, BASE_KEY "/yes/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "../yes/**", KEY_END),
		   keyNew (BASE_KEY "/yes/target", KEY_END), KS_END)
	TEST_SET (positive, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (positive)

	WITH_KEYS (empty_positive, 2,
		   keyNew (BASE_KEY "/ref", KEY_VALUE, "", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_META,
			   CHECK_REFERENCE_RESTRICT_KEYNAME, "", KEY_END),
		   KS_END)
	TEST_SET (empty_positive, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (empty_positive)

	WITH_KEYS (negative, 2,
		   keyNew (BASE_KEY "/ref", KEY_VALUE, BASE_KEY "/no/target", KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "../yes/**", KEY_END),
		   keyNew (BASE_KEY "/no/target", KEY_END), KS_END)
	TEST_SET (negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (negative)

	WITH_KEYS (empty_negative, 2,
		   keyNew (BASE_KEY "/ref", KEY_VALUE, BASE_KEY "/target", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE,
			   KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "", KEY_END),
		   keyNew (BASE_KEY "/target", KEY_END), KS_END)
	TEST_SET (empty_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (empty_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (empty_negative)

	TEARDOWN ()
}

static void test_basics (void)
{
	SETUP (basics)

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	ksDel (ks);

	TEARDOWN ()
}


int main (int argc, char ** argv)
{
	printf ("REFERENCE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	test_single_positive ();
	test_single_negative ();

	test_recursive_positive ();
	test_recursive_negative ();

	test_restriction ();

	print_result ("testmod_reference");

	return nbError;
}
