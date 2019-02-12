/**
 * @file
 *
 * @brief Tests for data type checker
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "ctype.h"

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <tests_plugin.h>

static bool checkType (const Key * key)
{
	return elektraCTypeCheckType (key);
}

void test_validate (void)
{
	Key * parentKey = keyNew ("system/elektra/modules/ctype", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("", KEY_END);
	Plugin * plugin = elektraPluginOpen ("ctype", modules, conf, errorKey);
	keyDel (errorKey);
	exit_if_fail (plugin != 0, "could not open ctype plugin");

	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key;
	key = ksLookupByName (ks, "system/elektra/modules/ctype/exports/validateKey", 0);
	exit_if_fail (key, "key not found");

	union
	{
		int (*f) (Key *, Key *);
		void * v;
	} conversation;

	exit_if_fail (keyGetBinary (key, &conversation.v, sizeof (conversation)) == sizeof (conversation),
		      "failed to load validateKey function");

	Key * k = keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
	succeed_if (conversation.f (k, parentKey), "should validate as short");
	succeed_if (keyGetMeta (parentKey, "error/number") == NULL, "shouldn't find error number");

	keySetString (k, "a");
	succeed_if (!conversation.f (k, parentKey), "shouldn't validate as short");
	succeed_if (keyGetMeta (parentKey, "error/number") != NULL, "should find error number");

	keyDel (k);

	ksDel (ks);
	keyDel (parentKey);

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_short (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "-32768");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "-32769");
	succeed_if (!checkType (k), "should fail (number too low)");
	keySetString (k, "32768");
	succeed_if (!checkType (k), "should fail (number too high)");
	keySetString (k, "32767");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "32767x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");
	keySetString (k, "32767 x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");

	keyDel (k);
}

void test_unsignedShort (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "unsigned_short", KEY_END);
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "0");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "-1");
	succeed_if (!checkType (k), "should fail (number too low)");
	keySetString (k, "65536");
	succeed_if (!checkType (k), "should fail (number too high)");
	keySetString (k, "65535");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "32767x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");
	keySetString (k, "32767 x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");

	keyDel (k);
}

void test_float (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "float", KEY_END);
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "0");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "1.5");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "1,5");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "1233322.5");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "123233223322333322.5");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "123233223322333322.0001");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, ".");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "1.");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, ".5");
	succeed_if (checkType (k), "should check successfully");

	keyDel (k);
}

void test_bool (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "boolean", KEY_END);
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "1");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, ".");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "true");
	succeed_if (!checkType (k), "should fail");
	keySetString (k, "false");
	succeed_if (!checkType (k), "should fail");

	keyDel (k);
}

void test_none (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "anything", KEY_END);
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "1");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "any other");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "do not care");
	succeed_if (checkType (k), "should check successfully");
	keySetString (k, "");
	succeed_if (checkType (k), "should check successfully");

	keyDel (k);
}

void test_char (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "a", KEY_META, "check/type", "char", KEY_END);
	succeed_if (checkType (k), "a should check successfully as char");
	keySetString (k, "b");
	succeed_if (checkType (k), "b should check successfully as char");
	keySetString (k, " ");
	succeed_if (checkType (k), "space should check successfully as char");
	keySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as char");
	keySetString (k, "ab");
	succeed_if (!checkType (k), "two chars should not check successfully as char");

	char x[2] = { 0, 0 };
	char msg[] = "\\xXX should check successfully as char";
	const char hex[] = "0123456789abcdef";
	for (int i = 1; i < 255; ++i)
	{
		x[0] = (char) i;
		keySetString (k, x);
		msg[2] = hex[i / 16];
		msg[3] = hex[i % 16];
		succeed_if (checkType (k), msg);
	}


	keyDel (k);
}

void test_octet (void)
{
	Key * k = keyNew ("user/anything", KEY_VALUE, "a", KEY_META, "check/type", "octet", KEY_END);
	succeed_if (checkType (k), "a should check successfully as octet");
	keySetString (k, "b");
	succeed_if (checkType (k), "b should check successfully as octet");
	keySetString (k, " ");
	succeed_if (checkType (k), "space should check successfully as octet");
	keySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as octet");
	keySetString (k, "ab");
	succeed_if (!checkType (k), "two chars should not check successfully as octet");

	char x[2] = { 0, 0 };
	char msg[] = "\\xXX should check successfully as octet";
	const char hex[] = "0123456789abcdef";
	for (int i = 1; i < 255; ++i)
	{
		x[0] = (char) i;
		keySetString (k, x);
		msg[2] = hex[i / 16];
		msg[3] = hex[i % 16];
		succeed_if (checkType (k), msg);
	}

	keyDel (k);
}

static void test_enum (void)
{
	Key * parentKey = keyNew ("user/tests/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user/tests/enum/valid1", KEY_VALUE, "LOW", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#1",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_END);
	Key * k2 = keyNew ("user/tests/enum/valid2", KEY_VALUE, "LOW MIDDLE", KEY_META, "check/enum/multi", " ", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE",
			   KEY_META, "check/enum/#2", "HIGH", KEY_END);
	Key * k3 = keyNew ("user/tests/enum/invalid1", KEY_VALUE, "HIGH", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#1",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_END);
	Key * k4 = keyNew ("user/tests/enum/invalid2", KEY_VALUE, "LOW FAIL", KEY_META, "check/enum/multi", " ", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE",
			   KEY_META, "check/enum/#2", "HIGH", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("ctype");

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k1);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k4);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumMulti (void)
{
	Key * parentKey = keyNew ("user/tests/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user/tests/enum/valid1", KEY_VALUE, "LOW", KEY_META, "check/enum/multi", "_", KEY_META, "check/type", "enum",
			   KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_END);
	Key * k2 = keyNew ("user/tests/enum/valid2", KEY_VALUE, "LOW_MIDDLE", KEY_META, "check/enum/multi", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE",
			   KEY_END);
	Key * k3 = keyNew ("user/tests/enum/invalid1", KEY_VALUE, "HIGH", KEY_META, "check/enum/multi", "_", KEY_META, "check/type", "enum",
			   KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_END);
	Key * k4 = keyNew ("user/tests/enum/invalid2", KEY_VALUE, "MIDDLE_FAIL", KEY_META, "check/enum/multi", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE",
			   KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks;
	PLUGIN_OPEN ("ctype");

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k1);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (1), "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k4);
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("CTYPE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_validate ();
	test_short ();
	test_unsignedShort ();
	test_float ();
	test_bool ();
	test_none ();
	test_char ();
	test_octet ();

	test_enum ();
	test_enumMulti ();

	print_result ("testmod_ctype");

	return nbError;
}
