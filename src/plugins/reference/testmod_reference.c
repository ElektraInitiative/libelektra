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
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "full reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative1", "../../target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative1 reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative2", "./target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/ref/relative2/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative2 reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/relative3", "@/ref/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/ref/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "relative2 reference failed");
	ksDel (ks);

	ks = ksNew (
		5, keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/target1", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "array reference failed");
	ksDel (ks);

	// negative tests
	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/full", BASE_KEY "/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "full-negative reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative1", "../../target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative1-negative reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative2", "./target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/ref/relative2/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative2-negative reference failed");
	ksDel (ks);

	ks = ksNew (2, keyNewReference (BASE_KEY "/ref/neg/relative3", "@/ref/target", CHECK_REFERNCE_VALUE_SINGLE, NULL),
		    keyNew (BASE_KEY "/hidden/ref/target", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "relative2-negative reference failed");
	ksDel (ks);

	ks = ksNew (
		5, keyNew (BASE_KEY "/ref/array", KEY_VALUE, "#1", KEY_META, CHECK_REFERENCE_KEYNAME, CHECK_REFERNCE_VALUE_SINGLE, KEY_END),
		keyNew (BASE_KEY "/ref/array/#0", KEY_VALUE, BASE_KEY "/target0", KEY_END),
		keyNew (BASE_KEY "/ref/array/#1", KEY_VALUE, BASE_KEY "/target1", KEY_END), keyNew (BASE_KEY "/hidden/target0", KEY_END),
		keyNew (BASE_KEY "/hidden/target1", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "array-negative reference failed");
	ksDel (ks);

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

	print_result ("testmod_reference");

	return nbError;
}
