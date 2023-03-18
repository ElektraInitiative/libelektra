/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "type.h"

#include <elektra/plugin/plugin.h>
#include <internal/pluginload/module.h>
#include <tests_plugin.h>

static bool checkType (const Key * key)
{
	return elektraTypeCheckType (key);
}

void test_validate (void)
{
	Key * parentKey = keyNew ("system:/elektra/modules/type", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("type");

	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	Key * key;
	key = ksLookupByName (ks, "system:/elektra/modules/type/exports/validateKey", 0);
	exit_if_fail (key, "key not found");

	union
	{
		int (*f) (Plugin *, Key *, Key *);
		void * v;
	} conversation;

	exit_if_fail (keyGetBinary (key, &conversation.v, sizeof (conversation)) == sizeof (conversation),
		      "failed to load validateKey function");

	Key * k = keyNew ("user:/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
	succeed_if (conversation.f (plugin, k, parentKey), "should validate as short");
	succeed_if (keyGetMeta (parentKey, "error/number") == NULL, "shouldn't find error number");

	keySetString (k, "a");
	succeed_if (!conversation.f (plugin, k, parentKey), "shouldn't validate as short");
	succeed_if (keyGetMeta (parentKey, "error/number") != NULL, "should find error number");

	keyDel (k);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_short (void)
{
	Key * k = keyNew ("user:/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
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
	Key * k = keyNew ("user:/anything", KEY_VALUE, "0", KEY_META, "check/type", "unsigned_short", KEY_END);
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
	Key * k = keyNew ("user:/anything", KEY_VALUE, "0", KEY_META, "check/type", "float", KEY_END);
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
	Key * k = keyNew ("user:/anything", KEY_VALUE, "0", KEY_META, "check/type", "boolean", KEY_END);
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
	Key * k = keyNew ("user:/anything", KEY_VALUE, "anything", KEY_END);
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
	Key * k = keyNew ("user:/anything", KEY_VALUE, "a", KEY_META, "check/type", "char", KEY_END);
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

void test_wchar (void)
{
	Key * k = keyNew ("user:/anything", KEY_VALUE, "a", KEY_META, "check/type", "wchar", KEY_END);
	keySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as wchar");

	char * s = elektraCalloc (3 * MB_CUR_MAX + 1);
	wcstombs (s, L"ab", 3 * MB_CUR_MAX);
	keySetString (k, s);
	succeed_if (!checkType (k), "two wchars should not check successfully as wchar");

	size_t max = WCHAR_MAX;
	if (max > 0xFFFF)
	{
		max = 0xFFFF;
	}

	for (size_t i = 1; i < max; ++i)
	{
		size_t ret = wcstombs (s, (wchar_t[]){ (wchar_t) i, 0 }, 3 * MB_CUR_MAX);
		if (ret > 0)
		{
			keySetString (k, s);
			nbTest++;
			if (!checkType (k))
			{
				yield_error ("the following should check successfully as wchar:");
				printf ("0x%zx\n", i);
			}
		}
		int x ELEKTRA_UNUSED = wctomb (NULL, 0);
	}

	elektraFree (s);

	keyDel (k);
}


void test_octet (void)
{
	Key * k = keyNew ("user:/anything", KEY_VALUE, "a", KEY_META, "check/type", "octet", KEY_END);
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

void test_string (void)
{
	Key * k = keyNew ("user:/anything", KEY_VALUE, "a", KEY_META, "check/type", "string", KEY_END);
	keySetString (k, "");
	succeed_if (checkType (k), "empty string should check successfully as string");

	keySetString (k, "ab");
	succeed_if (checkType (k), "\"ab\" should check successfully as string");

	keySetString (k, "monkey circus");
	succeed_if (checkType (k), "\"monkey circus\" should check successfully as string");

	keySetString (k, "日本");
	succeed_if (checkType (k), "\"日本\" should check successfully as string");

	keySetString (k, "😂 😄 😃 😀 😊 😉 😍 😘 😚 😗 😙 😜 😝 😛 😳");
	succeed_if (checkType (k), "emoji sequence should check successfully as string");

	keyDel (k);
}

void test_wstring (void)
{
	Key * k = keyNew ("user:/anything", KEY_VALUE, "a", KEY_META, "check/type", "wstring", KEY_END);
	keySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as wstring");

	char s[255 * MB_CUR_MAX];
	wcstombs (s, L"ab", sizeof (s));
	keySetString (k, s);
	succeed_if (checkType (k), "two wchars should check successfully as wstring");

	wcstombs (s, L"monkey circus", sizeof (s));
	keySetString (k, s);
	succeed_if (checkType (k), "L\"monkey circus\" should check successfully as wstring");

	wcstombs (s, L"日本", sizeof (s));
	keySetString (k, s);
	succeed_if (checkType (k), "\"日本\" should check successfully as wstring");

	wcstombs (s, L"😂 😄 😃 😀 😊 😉 😍 😘 😚 😗 😙 😜 😝 😛 😳", sizeof (s));
	keySetString (k, s);
	succeed_if (checkType (k), "emoji sequence should check successfully as wstring");

	keyDel (k);
}


static void test_enum (void)
{
	Key * parentKey = keyNew ("user:/tests/type/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/type/enum/valid1", KEY_VALUE, "LOW", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#1",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_END);
	Key * k2 =
		keyNew ("user:/tests/type/enum/valid2", KEY_VALUE, "MIDDLE", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#2",
			KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_META, "check/enum/#2", "HIGH", KEY_END);
	Key * k3 = keyNew ("user:/tests/type/enum/valid3", KEY_VALUE, "HIGH", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#2",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#2", "HIGH", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumMulti (void)
{
	Key * parentKey = keyNew ("user:/tests/type/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/type/enum/valid1", KEY_VALUE, "LOW", KEY_META, "check/enum/delimiter", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE",
			   KEY_END);
	Key * k2 = keyNew ("user:/tests/type/enum/valid2", KEY_VALUE, "LOW_MIDDLE", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1",
			   "MIDDLE", KEY_END);
	Key * k3 = keyNew ("user:/tests/type/enum/invalid1", KEY_VALUE, "HIGH", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1",
			   "MIDDLE", KEY_END);
	Key * k4 = keyNew ("user:/tests/type/enum/invalid2", KEY_VALUE, "MIDDLE_FAIL", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#1", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1",
			   "MIDDLE", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (3, k1, k2, KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k4);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumNormalize (void)
{
	Key * parentKey = keyNew ("user:/tests/type/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/type/enum/valid1", KEY_VALUE, "LOW", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#1",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_META, "check/enum/normalize", "1",
			   KEY_END);
	Key * k2 = keyNew ("user:/tests/type/enum/valid2", KEY_VALUE, "MIDDLE", KEY_META, "check/type", "enum", KEY_META, "check/enum",
			   "#2", KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#1", "MIDDLE", KEY_META, "check/enum/#2", "HIGH",
			   KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k3 = keyNew ("user:/tests/type/enum/valid3", KEY_VALUE, "HIGH", KEY_META, "check/type", "enum", KEY_META, "check/enum", "#2",
			   KEY_META, "check/enum/#0", "LOW", KEY_META, "check/enum/#2", "HIGH", KEY_META, "check/enum/normalize", "1",
			   KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (keyString (k1), "0");
	succeed_if_same_string (keyString (k2), "1");
	succeed_if_same_string (keyString (k3), "2");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (keyString (k1), "LOW");
	succeed_if_same_string (keyString (k2), "MIDDLE");
	succeed_if_same_string (keyString (k3), "HIGH");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (keyString (k1), "0");
	succeed_if_same_string (keyString (k2), "1");
	succeed_if_same_string (keyString (k3), "2");

	keySetString (k1, "0");
	keySetString (k2, "LOW");
	keySetString (k3, "0");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (keyString (k1), "LOW");
	succeed_if_same_string (keyString (k2), "LOW");
	succeed_if_same_string (keyString (k3), "LOW");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumMultiNormalize (void)
{
	Key * parentKey = keyNew ("user:/tests/type/enum", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/type/enum/valid1", KEY_VALUE, "none", KEY_META, "check/enum/delimiter", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "low",
			   KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k2 = keyNew ("user:/tests/type/enum/valid2", KEY_VALUE, "low", KEY_META, "check/enum/delimiter", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "low",
			   KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k3 = keyNew ("user:/tests/type/enum/valid3", KEY_VALUE, "high", KEY_META, "check/enum/delimiter", "_", KEY_META, "check/type",
			   "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "low",
			   KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k4 = keyNew ("user:/tests/type/enum/valid4", KEY_VALUE, "high_low", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1",
			   "low", KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k5 = keyNew ("user:/tests/type/enum/valid5", KEY_VALUE, "none_low", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1",
			   "low", KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k6 = keyNew ("user:/tests/type/enum/valid6", KEY_VALUE, "low_low_high", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1",
			   "low", KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	Key * k7 = keyNew ("user:/tests/type/enum/valid7", KEY_VALUE, "low_high", KEY_META, "check/enum/delimiter", "_", KEY_META,
			   "check/type", "enum", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1",
			   "low", KEY_META, "check/enum/#2", "high", KEY_META, "check/enum/normalize", "1", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (7, k1, k2, k3, k4, k5, k6, k7, KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (keyString (k1), "0");
	succeed_if_same_string (keyString (k2), "1");
	succeed_if_same_string (keyString (k3), "2");
	succeed_if_same_string (keyString (k4), "3");
	succeed_if_same_string (keyString (k5), "1");
	succeed_if_same_string (keyString (k6), "3");
	succeed_if_same_string (keyString (k7), "3");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (keyString (k1), "none");
	succeed_if_same_string (keyString (k2), "low");
	succeed_if_same_string (keyString (k3), "high");
	succeed_if_same_string (keyString (k4), "high_low");
	succeed_if_same_string (keyString (k5), "none_low");
	succeed_if_same_string (keyString (k6), "low_low_high");
	succeed_if_same_string (keyString (k7), "low_high");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (keyString (k1), "0");
	succeed_if_same_string (keyString (k2), "1");
	succeed_if_same_string (keyString (k3), "2");
	succeed_if_same_string (keyString (k4), "3");
	succeed_if_same_string (keyString (k5), "1");
	succeed_if_same_string (keyString (k6), "3");
	succeed_if_same_string (keyString (k7), "3");

	keySetString (k1, "3");
	keySetString (k2, "0");
	keySetString (k3, "1");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (keyString (k1), "high_low");
	succeed_if_same_string (keyString (k2), "none");
	succeed_if_same_string (keyString (k3), "low");
	succeed_if_same_string (keyString (k4), "high_low");
	succeed_if_same_string (keyString (k5), "none_low");
	succeed_if_same_string (keyString (k6), "low_low_high");
	succeed_if_same_string (keyString (k7), "low_high");


	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_booleanDefault (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t2", 0)), "1");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f2", 0)), "0");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/on", 0)), "1");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/off", 0)), "0");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanDefaultRestore (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/a0", KEY_VALUE, "x", KEY_END),
			     keyNew ("user:/tests/type/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t2", 0)), "tRUe");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanDefaultError (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/t2", KEY_VALUE, "tRUe", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/nt", KEY_VALUE, "i'm not true", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f1", KEY_VALUE, "false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f2", KEY_VALUE, "falsE", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/nf", KEY_VALUE, "i'm not false", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/off", KEY_VALUE, "off", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/on", KEY_VALUE, "on", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet was not successful");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValue (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("user:/booleans", KEY_VALUE, "#1", KEY_END),
			       keyNew ("user:/booleans/#0/true", KEY_VALUE, "strangeTrueValue", KEY_END),
			       keyNew ("user:/booleans/#0/false", KEY_VALUE, "0", KEY_END),
			       keyNew ("user:/booleans/#1/true", KEY_VALUE, "1", KEY_END),
			       keyNew ("user:/booleans/#1/false", KEY_VALUE, "strangeFalseValue", KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/t1", KEY_VALUE, "strangeTrueValue", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f1", KEY_VALUE, "strangeFalseValue", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "strangeTrueValue");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f1", 0)), "strangeFalseValue");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValueWeird (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("user:/booleans", KEY_VALUE, "#3", KEY_END),
			       keyNew ("user:/booleans/#0/true", KEY_VALUE, "strangeTrueValue", KEY_END),
			       keyNew ("user:/booleans/#0/false", KEY_VALUE, "0", KEY_END),
			       keyNew ("user:/booleans/#1/true", KEY_VALUE, "1", KEY_END),
			       keyNew ("user:/booleans/#1/false", KEY_VALUE, "strangeFalseValue", KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/t1", KEY_VALUE, "strangeTrueValue", KEY_META, type, "boolean", KEY_END),
			     keyNew ("user:/tests/type/f1", KEY_VALUE, "strangeFalseValue", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "strangeTrueValue");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/f1", 0)), "strangeFalseValue");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValueError (void)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("user:/booleans", KEY_VALUE, "#1", KEY_END),
			       keyNew ("user:/booleans/#0/true", KEY_VALUE, "strangeTrueValue", KEY_END),
			       keyNew ("user:/booleans/#1/true", KEY_VALUE, "1", KEY_END),
			       keyNew ("user:/booleans/#1/false", KEY_VALUE, "strangeFalseValue", KEY_END), KS_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen ("type", modules, conf, errorKey);
	succeed_if (plugin == NULL, "plugin open should have failed");
	succeed_if (keyGetMeta (errorKey, "error") != NULL, "no error found after failed open");
	succeed_if_same_string (keyString (keyGetMeta (errorKey, "error/reason")),
				"You must set both true and false for a boolean pair (config key: '/booleans/#0')");
	keyDel (errorKey);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanChangeValue (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("type");
	KeySet * ks = ksNew (30, keyNew ("user:/tests/type/t1", KEY_VALUE, "0", KEY_META, type, "boolean", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	keySetString (ksLookupByName (ks, "user:/tests/type/t1", 0), "yes");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "yes");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	keySetString (ksLookupByName (ks, "user:/tests/type/t1", 0), "1");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanOverride (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/boolean/restoreas", KEY_VALUE, "#0", KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	Key * k1 = keyNew ("user:/tests/type/t1", KEY_VALUE, "t", KEY_META, type, "boolean", KEY_META, "check/boolean/true", "t", KEY_META,
			   "check/boolean/false", "f", KEY_END);
	KeySet * ks = ksNew (30, k1, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	keySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	keySetString (k1, "t");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreAsDefault (const char * type, const char * restore, const char * restoreTrue, const char * restoreFalse)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/boolean/restoreas", KEY_VALUE, restore, KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	Key * k1 = keyNew ("user:/tests/type/t1", KEY_VALUE, "enabled", KEY_META, type, "boolean", KEY_END);
	KeySet * ks = ksNew (30, k1, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), restoreTrue);

	keySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), restoreTrue);

	keySetString (k1, "false");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), restoreFalse);

	keySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), restoreFalse);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreAs (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf =
		ksNew (4, keyNew ("user:/boolean/restoreas", KEY_VALUE, "#0", KEY_END), keyNew ("user:/booleans", KEY_VALUE, "#0", KEY_END),
		       keyNew ("user:/booleans/#0/true", KEY_VALUE, "t", KEY_END),
		       keyNew ("user:/booleans/#0/false", KEY_VALUE, "f", KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	Key * k1 = keyNew ("user:/tests/type/t1", KEY_VALUE, "t", KEY_META, type, "boolean", KEY_END);
	KeySet * ks = ksNew (30, k1, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	keySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	keySetString (k1, "f");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "f");

	keySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "f");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreDisabled (const char * type)
{
	Key * parentKey = keyNew ("user:/tests/type", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/boolean/restoreas", KEY_VALUE, "none", KEY_END), KS_END);
	PLUGIN_OPEN ("type");
	Key * k1 = keyNew ("user:/tests/type/t1", KEY_VALUE, "true", KEY_META, type, "boolean", KEY_END);
	KeySet * ks = ksNew (30, k1, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	keySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	keySetString (k1, "false");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	keySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("TYPE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_validate ();
	test_short ();
	test_unsignedShort ();
	test_float ();
	test_bool ();
	test_none ();
	test_char ();
	test_wchar ();
	test_octet ();
	test_string ();
	test_wstring ();

	test_enum ();
	test_enumMulti ();

	test_enumNormalize ();
	test_enumMultiNormalize ();

	test_booleanDefault ("type");
	test_booleanDefaultError ("type");
	test_booleanDefaultRestore ("type");
	test_booleanUserValue ("type");
	test_booleanUserValueWeird ("type");
	test_booleanChangeValue ("type");
	test_booleanOverride ("type");

	test_booleanDefault ("check/type");
	test_booleanDefaultError ("check/type");
	test_booleanDefaultRestore ("check/type");
	test_booleanUserValue ("check/type");
	test_booleanUserValueWeird ("check/type");
	test_booleanChangeValue ("check/type");
	test_booleanOverride ("check/type");

	test_booleanRestoreDisabled ("type");
	test_booleanRestoreDisabled ("check/type");

	test_booleanRestoreAs ("type");
	test_booleanRestoreAs ("check/type");

	test_booleanRestoreAsDefault ("type", "#0", "yes", "no");
	test_booleanRestoreAsDefault ("type", "#1", "true", "false");
	test_booleanRestoreAsDefault ("type", "#2", "on", "off");
	test_booleanRestoreAsDefault ("type", "#3", "enabled", "disabled");
	test_booleanRestoreAsDefault ("type", "#4", "enable", "disable");

	test_booleanRestoreAsDefault ("check/type", "#0", "yes", "no");
	test_booleanRestoreAsDefault ("check/type", "#1", "true", "false");
	test_booleanRestoreAsDefault ("check/type", "#2", "on", "off");
	test_booleanRestoreAsDefault ("check/type", "#3", "enabled", "disabled");
	test_booleanRestoreAsDefault ("check/type", "#4", "enable", "disable");

	test_booleanUserValueError ();

	print_result ("testmod_type");

	return nbError;
}
