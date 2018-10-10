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

#include <kdbconfig.h>

#include <tests_plugin.h>

#include "reference.h"

#define BASE_KEY "user/tests/reference"

#define xstr(a) str(a)
#define str(a) #a
#define check_error(PARENT_KEY, ERROR_CODE, MESSAGE) succeed_if (check_error0(PARENT_KEY, xstr(ERROR_CODE)), MESSAGE)

static inline bool check_error0(const Key * parentKey, const char * expectedError)
{
	const Key * errorKey = keyGetMeta (parentKey, "error/number");
	const char * actualError = errorKey != NULL ? keyString (errorKey) : NULL;
	return actualError != NULL && strcmp (actualError, expectedError) == 0;
}
static Key * keyNewReference (const char * name, const char * target, const char * type, const char * restriction)
{
	if (restriction != NULL)
	{
		return keyNew (name, KEY_VALUE, target, KEY_META, CHECK_REFERENCE_KEYNAME, type, KEY_META, CHECK_REFERENCE_RESTRICT_KEYNAME,
			       restriction, KEY_END);
	}
	else
	{
		return keyNew (name, KEY_VALUE, target, KEY_META, CHECK_REFERENCE_KEYNAME, type, KEY_END);
	}
}

static void test_single_reference (void)
{
	printf ("test single reference\n");

	Key * parentKey = keyNew (BASE_KEY, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("reference");

	// positive tests
	KeySet * ks = ksNew (2, keyNewReference (BASE_KEY "/ref/full", BASE_KEY "/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
			     keyNew (BASE_KEY "/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "full: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative1", "../../target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative1: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative2", "./target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/ref/relative2/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative2: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative3", "@/ref/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/ref/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative3: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (
		5, keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/target1", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "array: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	// negative tests
	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/full", BASE_KEY "/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "full-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "full-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative1", "../../target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative1-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "relative1-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative2", "./target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/ref/relative2/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative2-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "relative2-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative3", "@/ref/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/ref/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative3-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "relative3-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (
		5, keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/hidden/target0", KEY_END),
		keyNew (BASE_KEY "/hidden/target1", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "array-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "array-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_recursive_reference (void)
{
	printf ("test recursive reference\n");

	Key * parentKey = keyNew (BASE_KEY, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("reference");

	// positive tests
	KeySet * ks = ksNew (9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
			     keyNewReference (BASE_KEY "/head/ref", BASE_KEY "/element0", CHECK_REFERNCE_VALUE_RECURSIVE, NULL),
			     keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
			     keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element1", KEY_END),
			     keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
			     keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
			     keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
			     keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/element3", KEY_END),
			     keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "linked-list: failed");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	// negative tests
	ks = ksNew (9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		    keyNewReference (BASE_KEY "/head/ref", BASE_KEY "/element0", CHECK_REFERNCE_VALUE_RECURSIVE, NULL),
		    keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		    keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element4", KEY_END),
		    keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		    keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		    keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		    keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/element3", KEY_END),
		    keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "linked-list-negative: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_NOT_FOUND, "linked-list-negative: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	ks = ksNew (9, keyNew (BASE_KEY "/head", KEY_VALUE, "head", KEY_END),
		    keyNewReference (BASE_KEY "/head/ref", BASE_KEY "/element0", CHECK_REFERNCE_VALUE_RECURSIVE, NULL),
		    keyNew (BASE_KEY "/element0", KEY_VALUE, "element0", KEY_END),
		    keyNew (BASE_KEY "/element0/ref", KEY_VALUE, BASE_KEY "/element1", KEY_END),
		    keyNew (BASE_KEY "/element1", KEY_VALUE, "element1", KEY_END),
		    keyNew (BASE_KEY "/element1/ref", KEY_VALUE, BASE_KEY "/element2", KEY_END),
		    keyNew (BASE_KEY "/element2", KEY_VALUE, "element2", KEY_END),
		    keyNew (BASE_KEY "/element2/ref", KEY_VALUE, BASE_KEY "/head", KEY_END),
		    keyNew (BASE_KEY "/element3", KEY_VALUE, "element3", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "linked-list-negative2: failed");
	check_error (parentKey, ELEKTRA_ERROR_REFERENCE_CYCLIC_GRAPH, "linked-list-negative2: wrong error");
	ksDel (ks);
	keySetMeta (parentKey, "error", NULL);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew (BASE_KEY, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("reference");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("REFERENCE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_single_reference ();
	test_recursive_reference ();

	print_result ("testmod_reference");

	return nbError;
}
