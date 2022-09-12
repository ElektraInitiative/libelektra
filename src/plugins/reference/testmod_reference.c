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

#include <kdbmacros.h>

#include <tests_plugin.h>

#include "reference.h"

#define BASE_KEY "user:/tests/reference"

#define xstr(a) str (a)
#define str(a) #a

#define SETUP(name)                                                                                                                        \
	printf ("test %s\n", #name);                                                                                                       \
                                                                                                                                           \
	ElektraKey * parentKey = keyNew (BASE_KEY, ELEKTRA_KEY_END);                                                                               \
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);                                                                                          \
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
	ElektraKeyset * NAME##_keys = ksNew (COUNT, __VA_ARGS__, ELEKTRA_KS_END);

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

static bool check_error0 (const ElektraKey * parentKey, const char * expectedError)
{
	const ElektraKey * errorKey = keyGetMeta (parentKey, "error/number");
	const char * actualError = errorKey != NULL ? keyString (errorKey) : NULL;
	return actualError != NULL && strcmp (actualError, expectedError) == 0;
}

static void test_single_negative (void)
{
	SETUP (single positive)

	WITH_KEYS (full, 2,
		   keyNew (BASE_KEY "/ref/full", ELEKTRA_KEY_VALUE, BASE_KEY "/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/target", ELEKTRA_KEY_END))
	TEST_SET (full, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (full)

	WITH_KEYS (relative1, 2,
		   keyNew (BASE_KEY "/ref/relative1", ELEKTRA_KEY_VALUE, "../../target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/target", ELEKTRA_KEY_END))
	TEST_SET (relative1, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative1)

	WITH_KEYS (relative2, 2,
		   keyNew (BASE_KEY "/ref/relative2", ELEKTRA_KEY_VALUE, "./target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE,
			   ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/relative2/target", ELEKTRA_KEY_END))
	TEST_SET (relative2, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative2)

	WITH_KEYS (relative3, 2,
		   keyNew (BASE_KEY "/ref/relative3", ELEKTRA_KEY_VALUE, "@/ref/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/target", ELEKTRA_KEY_END))
	TEST_SET (relative3, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (relative3)

	WITH_KEYS (array, 5,
		   keyNew (BASE_KEY "/ref/array", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/array/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/target0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/array/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/target1", ELEKTRA_KEY_END), keyNew (BASE_KEY "/target0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/target1", ELEKTRA_KEY_END))
	TEST_SET (array, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (array)

	TEARDOWN ()
}

static void test_single_positive (void)
{
	SETUP (single positive)

	WITH_KEYS (full_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/full", ELEKTRA_KEY_VALUE, BASE_KEY "/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/hidden/target", ELEKTRA_KEY_END))
	TEST_SET (full_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (full_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (full_negative)

	WITH_KEYS (relative1_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative1", ELEKTRA_KEY_VALUE, "../../target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/hidden/target", ELEKTRA_KEY_END))
	TEST_SET (relative1_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative1_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative1_negative)

	WITH_KEYS (relative2_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative2", ELEKTRA_KEY_VALUE, "./target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/hidden/ref/relative2/target", ELEKTRA_KEY_END))
	TEST_SET (relative2_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative2_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative2_negative)

	WITH_KEYS (relative3_negative, 2,
		   keyNew (BASE_KEY "/ref/neg/relative3", ELEKTRA_KEY_VALUE, "@/ref/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/hidden/ref/target", ELEKTRA_KEY_END))
	TEST_SET (relative3_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (relative3_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (relative3_negative)

	WITH_KEYS (array_negative, 5,
		   keyNew (BASE_KEY "/ref/array", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/array/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/target0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/ref/array/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/target1", ELEKTRA_KEY_END), keyNew (BASE_KEY "/hidden/target0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/hidden/target1", ELEKTRA_KEY_END))
	TEST_SET (array_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (array_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (array_negative)

	TEARDOWN ();
}

static void test_recursive_positive (void)
{
	SETUP (recursive positive)

	WITH_KEYS (linked_list, 9, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element1", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2", ELEKTRA_KEY_VALUE, "element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element3", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element3", ELEKTRA_KEY_VALUE, "element3", ELEKTRA_KEY_END))
	TEST_SET (linked_list, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (linked_list)

	WITH_KEYS (
		tree, 15, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END), keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, "#2", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/element00", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/element01", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref/#2", ELEKTRA_KEY_VALUE, BASE_KEY "/element02", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element00", ELEKTRA_KEY_VALUE, "element00", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element01", ELEKTRA_KEY_VALUE, "element01", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element02", ELEKTRA_KEY_VALUE, "element02", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element02/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element020", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element020", ELEKTRA_KEY_VALUE, "element020", ELEKTRA_KEY_END))
	TEST_SET (tree, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (tree)

	WITH_KEYS (
		merge, 9, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element1/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element2", ELEKTRA_KEY_VALUE, "element2", ELEKTRA_KEY_END))
	TEST_SET (merge, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (merge)

	WITH_KEYS (
		alternative, 16, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/head/ref/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END), keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref/#0", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element0/ref/#1", ELEKTRA_KEY_VALUE, BASE_KEY "/element4", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element1/altref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			CHECK_REFERNCE_VALUE_ALTERNATIVE, ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element2", ELEKTRA_KEY_VALUE, "element2", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element2/altref", ELEKTRA_KEY_VALUE, BASE_KEY "/element3", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element3", ELEKTRA_KEY_VALUE, "element3", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element4", ELEKTRA_KEY_VALUE, "element4", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element4/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element5", ELEKTRA_KEY_END),
		keyNew (BASE_KEY "/element5", ELEKTRA_KEY_VALUE, "element5", ELEKTRA_KEY_END))
	TEST_SET (alternative, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (alternative)

	TEARDOWN ()
}

static void test_recursive_negative (void)
{
	SETUP (recursive negative)

	WITH_KEYS (linked_list_negative, 9, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element4", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2", ELEKTRA_KEY_VALUE, "element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element3", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element3", ELEKTRA_KEY_VALUE, "element3", ELEKTRA_KEY_END))
	TEST_SET (linked_list_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (linked_list_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	RESET (linked_list_negative)

	WITH_KEYS (linked_list_negative2, 9, keyNew (BASE_KEY "/head", ELEKTRA_KEY_VALUE, "head", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/head/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element0", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_RECURSIVE, ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0", ELEKTRA_KEY_VALUE, "element0", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element0/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element1", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1", ELEKTRA_KEY_VALUE, "element1", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element1/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2", ELEKTRA_KEY_VALUE, "element2", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element2/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/head", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/element3", ELEKTRA_KEY_VALUE, "element3", ELEKTRA_KEY_END))
	TEST_SET (linked_list_negative2, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (linked_list_negative2, ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	RESET (linked_list_negative2)

	TEARDOWN ()
}

static void test_restriction (void)
{
	SETUP (restriction positive)

	WITH_KEYS (positive, 2,
		   keyNew (BASE_KEY "/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/yes/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "../yes/**", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/yes/target", ELEKTRA_KEY_END), ELEKTRA_KS_END)
	TEST_SET (positive, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (positive)

	WITH_KEYS (empty_positive, 2,
		   keyNew (BASE_KEY "/ref", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_META,
			   CHECK_REFERENCE_RESTRICT_KEYNAME, "", ELEKTRA_KEY_END),
		   ELEKTRA_KS_END)
	TEST_SET (empty_positive, ELEKTRA_PLUGIN_STATUS_NO_UPDATE)
	RESET (empty_positive)

	WITH_KEYS (negative, 2,
		   keyNew (BASE_KEY "/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/no/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME,
			   CHECK_REFERNCE_VALUE_SINGLE, ELEKTRA_KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "../yes/**", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/no/target", ELEKTRA_KEY_END), ELEKTRA_KS_END)
	TEST_SET (negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (negative)

	WITH_KEYS (empty_negative, 2,
		   keyNew (BASE_KEY "/ref", ELEKTRA_KEY_VALUE, BASE_KEY "/target", ELEKTRA_KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE,
			   ELEKTRA_KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME, "", ELEKTRA_KEY_END),
		   keyNew (BASE_KEY "/target", ELEKTRA_KEY_END), ELEKTRA_KS_END)
	TEST_SET (empty_negative, ELEKTRA_PLUGIN_STATUS_ERROR)
	EXPECT_ERROR (empty_negative, ELEKTRA_ERROR_VALIDATION_SEMANTIC)
	RESET (empty_negative)

	TEARDOWN ()
}

static void test_basics (void)
{
	SETUP (basics);

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	ksDel (ks);

	TEARDOWN ();
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
