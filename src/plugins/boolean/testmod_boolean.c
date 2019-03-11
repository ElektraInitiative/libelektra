/*
 * *
 * @file
 *
 * @brief Tests for boolean plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_default (const char * type)
{
	Key * parentKey = keyNew ("user/tests/boolean", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("boolean");
	KeySet * ks = ksNew (30, keyNew ("user/tests/boolean/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/nt", KEY_VALUE, "i'm not true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/nf", KEY_VALUE, "i'm not false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "1"), "key t1 has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t2", 0)), "1"), "key t2 has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/nt", 0)), "1"), "key nt has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/f1", 0)), "0"), "key f1 has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/f2", 0)), "0"), "key f2 has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/nf", 0)), "1"), "key nf has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/on", 0)), "1"), "key on has wrong value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/off", 0)), "0"), "key off has wrong value");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_defaultRestore (const char * type)
{
	Key * parentKey = keyNew ("user/tests/boolean", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("boolean");
	KeySet * ks = ksNew (30, keyNew ("user/tests/boolean/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t2", 0)), "tRUe"), "restoring value in key t2 failed");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_defaultError (const char * type)
{
	Key * parentKey = keyNew ("user/tests/boolean", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("boolean");
	KeySet * ks = ksNew (30, keyNew ("user/tests/boolean/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/nt", KEY_VALUE, "i'm not true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/nf", KEY_VALUE, "i'm not false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user/tests/boolean/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "call to kdbSet was not successful");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_userValue (const char * type)
{
	Key * parentKey = keyNew ("user/tests/boolean", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/on/true", KEY_VALUE, "7", KEY_END),
			       keyNew ("system/true", KEY_VALUE, "strangeTrueValue", KEY_END), KS_END);
	PLUGIN_OPEN ("boolean");
	KeySet * ks =
		ksNew (30, keyNew ("user/tests/boolean/t1", KEY_VALUE, "strangeTrueValue", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "7"), "key t1 has wrong value");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "strangeTrueValue"), "key t1 has wrong value");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_changeValue (const char * type)
{
	Key * parentKey = keyNew ("user/tests/boolean", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("boolean");
	KeySet * ks = ksNew (30, keyNew ("user/tests/boolean/t1", KEY_VALUE, "0", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "0"), "key t1 has wrong value");

	keySetString (ksLookupByName (ks, "user/tests/boolean/t1", 0), "yes");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "yes"), "key t1 has wrong value");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/boolean/t1", 0)), "1"), "key t1 has wrong value");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("BOOLEAN     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_default ("type");
	test_defaultRestore ("type");
	test_defaultError ("type");
	test_userValue ("type");
	test_default ("check/type");
	test_defaultRestore ("check/type");
	test_defaultError ("check/type");
	test_userValue ("check/type");
	test_changeValue ("type");
	test_changeValue ("check/type");

	print_result ("testmod_boolean");

	return nbError;
}
