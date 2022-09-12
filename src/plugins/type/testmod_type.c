/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "type.h"

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <tests_plugin.h>

static bool checkType (const ElektraKey * key)
{
	return elektraTypeCheckType (key);
}

void test_validate (void)
{
	ElektraKey * parentKey = elektraKeyNew ("system:/elektra/modules/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);

	PLUGIN_OPEN ("type");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	ElektraKey * key;
	key = elektraKeysetLookupByName (ks, "system:/elektra/modules/type/exports/validateKey", 0);
	exit_if_fail (key, "key not found");

	union
	{
		int (*f) (Plugin *, ElektraKey *, ElektraKey *);
		void * v;
	} conversation;

	exit_if_fail (elektraKeyGetBinary (key, &conversation.v, sizeof (conversation)) == sizeof (conversation),
		      "failed to load validateKey function");

	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/type", "short", ELEKTRA_KEY_END);
	succeed_if (conversation.f (plugin, k, parentKey), "should validate as short");
	succeed_if (elektraKeyGetMeta (parentKey, "error/number") == NULL, "shouldn't find error number");

	elektraKeySetString (k, "a");
	succeed_if (!conversation.f (plugin, k, parentKey), "shouldn't validate as short");
	succeed_if (elektraKeyGetMeta (parentKey, "error/number") != NULL, "should find error number");

	elektraKeyDel (k);

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_short (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/type", "short", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "-32768");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "-32769");
	succeed_if (!checkType (k), "should fail (number too low)");
	elektraKeySetString (k, "32768");
	succeed_if (!checkType (k), "should fail (number too high)");
	elektraKeySetString (k, "32767");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "32767x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");
	elektraKeySetString (k, "32767 x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");

	elektraKeyDel (k);
}

void test_unsignedShort (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/type", "unsigned_short", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "0");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "-1");
	succeed_if (!checkType (k), "should fail (number too low)");
	elektraKeySetString (k, "65536");
	succeed_if (!checkType (k), "should fail (number too high)");
	elektraKeySetString (k, "65535");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "32767x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");
	elektraKeySetString (k, "32767 x");
	succeed_if (!checkType (k), "should fail because of garbage afterwards");

	elektraKeyDel (k);
}

void test_float (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/type", "float", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "0");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "1.5");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "1,5");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "1233322.5");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "123233223322333322.5");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "123233223322333322.0001");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "x");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, ".");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "1.");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, ".5");
	succeed_if (checkType (k), "should check successfully");

	elektraKeyDel (k);
}

void test_bool (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "check/type", "boolean", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "1");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, ".");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "true");
	succeed_if (!checkType (k), "should fail");
	elektraKeySetString (k, "false");
	succeed_if (!checkType (k), "should fail");

	elektraKeyDel (k);
}

void test_none (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "anything", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "1");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "any other");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "do not care");
	succeed_if (checkType (k), "should check successfully");
	elektraKeySetString (k, "");
	succeed_if (checkType (k), "should check successfully");

	elektraKeyDel (k);
}

void test_char (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "check/type", "char", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "a should check successfully as char");
	elektraKeySetString (k, "b");
	succeed_if (checkType (k), "b should check successfully as char");
	elektraKeySetString (k, " ");
	succeed_if (checkType (k), "space should check successfully as char");
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as char");
	elektraKeySetString (k, "ab");
	succeed_if (!checkType (k), "two chars should not check successfully as char");

	char x[2] = { 0, 0 };
	char msg[] = "\\xXX should check successfully as char";
	const char hex[] = "0123456789abcdef";
	for (int i = 1; i < 255; ++i)
	{
		x[0] = (char) i;
		elektraKeySetString (k, x);
		msg[2] = hex[i / 16];
		msg[3] = hex[i % 16];
		succeed_if (checkType (k), msg);
	}


	elektraKeyDel (k);
}

void test_wchar (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "check/type", "wchar", ELEKTRA_KEY_END);
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as wchar");

	char * s = elektraCalloc (3 * MB_CUR_MAX + 1);
	wcstombs (s, L"ab", 3 * MB_CUR_MAX);
	elektraKeySetString (k, s);
	succeed_if (!checkType (k), "two wchars should not check successfully as wchar");

	size_t max = WCHAR_MAX;
	if (max > 0xFFFF)
	{
		max = 0xFFFF;
	}

	size_t c = 0;
	for (size_t i = 1; i < max; ++i)
	{
		size_t ret = wcstombs (s, (wchar_t[]){ (wchar_t) i, 0 }, 3 * MB_CUR_MAX);
		if (ret > 0)
		{
			c++;
			elektraKeySetString (k, s);
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

	elektraKeyDel (k);
}


void test_octet (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "check/type", "octet", ELEKTRA_KEY_END);
	succeed_if (checkType (k), "a should check successfully as octet");
	elektraKeySetString (k, "b");
	succeed_if (checkType (k), "b should check successfully as octet");
	elektraKeySetString (k, " ");
	succeed_if (checkType (k), "space should check successfully as octet");
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as octet");
	elektraKeySetString (k, "ab");
	succeed_if (!checkType (k), "two chars should not check successfully as octet");

	char x[2] = { 0, 0 };
	char msg[] = "\\xXX should check successfully as octet";
	const char hex[] = "0123456789abcdef";
	for (int i = 1; i < 255; ++i)
	{
		x[0] = (char) i;
		elektraKeySetString (k, x);
		msg[2] = hex[i / 16];
		msg[3] = hex[i % 16];
		succeed_if (checkType (k), msg);
	}

	elektraKeyDel (k);
}

void test_string (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "check/type", "string", ELEKTRA_KEY_END);
	elektraKeySetString (k, "");
	succeed_if (checkType (k), "empty string should check successfully as string");

	elektraKeySetString (k, "ab");
	succeed_if (checkType (k), "\"ab\" should check successfully as string");

	elektraKeySetString (k, "monkey circus");
	succeed_if (checkType (k), "\"monkey circus\" should check successfully as string");

	elektraKeySetString (k, "日本");
	succeed_if (checkType (k), "\"日本\" should check successfully as string");

	elektraKeySetString (k, "😂 😄 😃 😀 😊 😉 😍 😘 😚 😗 😙 😜 😝 😛 😳");
	succeed_if (checkType (k), "emoji sequence should check successfully as string");

	elektraKeyDel (k);
}

void test_wstring (void)
{
	ElektraKey * k = elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "check/type", "wstring", ELEKTRA_KEY_END);
	elektraKeySetString (k, "");
	succeed_if (!checkType (k), "empty string should not check successfully as wstring");

	char s[255 * MB_CUR_MAX];
	wcstombs (s, L"ab", sizeof (s));
	elektraKeySetString (k, s);
	succeed_if (checkType (k), "two wchars should check successfully as wstring");

	wcstombs (s, L"monkey circus", sizeof (s));
	elektraKeySetString (k, s);
	succeed_if (checkType (k), "L\"monkey circus\" should check successfully as wstring");

	wcstombs (s, L"日本", sizeof (s));
	elektraKeySetString (k, s);
	succeed_if (checkType (k), "\"日本\" should check successfully as wstring");

	wcstombs (s, L"😂 😄 😃 😀 😊 😉 😍 😘 😚 😗 😙 😜 😝 😛 😳", sizeof (s));
	elektraKeySetString (k, s);
	succeed_if (checkType (k), "emoji sequence should check successfully as wstring");

	elektraKeyDel (k);
}


static void test_enum (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type/enum", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/enum/valid1", ELEKTRA_KEY_VALUE, "LOW", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#1",
			   ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1", "MIDDLE", ELEKTRA_KEY_END);
	ElektraKey * k2 =
		elektraKeyNew ("user:/tests/type/enum/valid2", ELEKTRA_KEY_VALUE, "MIDDLE", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2",
			ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1", "MIDDLE", ELEKTRA_KEY_META, "check/enum/#2", "HIGH", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/type/enum/valid3", ELEKTRA_KEY_VALUE, "HIGH", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2",
			   ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#2", "HIGH", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (3, k1, k2, k3, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumMulti (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type/enum", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/enum/valid1", ELEKTRA_KEY_VALUE, "LOW", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META, "check/type",
			   "enum", ELEKTRA_KEY_META, "check/enum", "#1", ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1", "MIDDLE",
			   ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/type/enum/valid2", ELEKTRA_KEY_VALUE, "LOW_MIDDLE", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#1", ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1",
			   "MIDDLE", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/type/enum/invalid1", ELEKTRA_KEY_VALUE, "HIGH", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#1", ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1",
			   "MIDDLE", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/type/enum/invalid2", ELEKTRA_KEY_VALUE, "MIDDLE_FAIL", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#1", ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1",
			   "MIDDLE", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (3, k1, k2, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k4);
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumNormalize (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type/enum", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/enum/valid1", ELEKTRA_KEY_VALUE, "LOW", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#1",
			   ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1", "MIDDLE", ELEKTRA_KEY_META, "check/enum/normalize", "1",
			   ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/type/enum/valid2", ELEKTRA_KEY_VALUE, "MIDDLE", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum",
			   "#2", ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#1", "MIDDLE", ELEKTRA_KEY_META, "check/enum/#2", "HIGH",
			   ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/type/enum/valid3", ELEKTRA_KEY_VALUE, "HIGH", ELEKTRA_KEY_META, "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2",
			   ELEKTRA_KEY_META, "check/enum/#0", "LOW", ELEKTRA_KEY_META, "check/enum/#2", "HIGH", ELEKTRA_KEY_META, "check/enum/normalize", "1",
			   ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (3, k1, k2, k3, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (elektraKeyString (k1), "0");
	succeed_if_same_string (elektraKeyString (k2), "1");
	succeed_if_same_string (elektraKeyString (k3), "2");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (elektraKeyString (k1), "LOW");
	succeed_if_same_string (elektraKeyString (k2), "MIDDLE");
	succeed_if_same_string (elektraKeyString (k3), "HIGH");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (elektraKeyString (k1), "0");
	succeed_if_same_string (elektraKeyString (k2), "1");
	succeed_if_same_string (elektraKeyString (k3), "2");

	elektraKeySetString (k1, "0");
	elektraKeySetString (k2, "LOW");
	elektraKeySetString (k3, "0");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (elektraKeyString (k1), "LOW");
	succeed_if_same_string (elektraKeyString (k2), "LOW");
	succeed_if_same_string (elektraKeyString (k3), "LOW");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_enumMultiNormalize (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type/enum", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/enum/valid1", ELEKTRA_KEY_VALUE, "none", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META, "check/type",
			   "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1", "low",
			   ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/type/enum/valid2", ELEKTRA_KEY_VALUE, "low", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META, "check/type",
			   "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1", "low",
			   ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/type/enum/valid3", ELEKTRA_KEY_VALUE, "high", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META, "check/type",
			   "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1", "low",
			   ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/type/enum/valid4", ELEKTRA_KEY_VALUE, "high_low", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1",
			   "low", ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k5 = elektraKeyNew ("user:/tests/type/enum/valid5", ELEKTRA_KEY_VALUE, "none_low", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1",
			   "low", ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k6 = elektraKeyNew ("user:/tests/type/enum/valid6", ELEKTRA_KEY_VALUE, "low_low_high", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1",
			   "low", ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKey * k7 = elektraKeyNew ("user:/tests/type/enum/valid7", ELEKTRA_KEY_VALUE, "low_high", ELEKTRA_KEY_META, "check/enum/delimiter", "_", ELEKTRA_KEY_META,
			   "check/type", "enum", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1",
			   "low", ELEKTRA_KEY_META, "check/enum/#2", "high", ELEKTRA_KEY_META, "check/enum/normalize", "1", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (7, k1, k2, k3, k4, k5, k6, k7, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (elektraKeyString (k1), "0");
	succeed_if_same_string (elektraKeyString (k2), "1");
	succeed_if_same_string (elektraKeyString (k3), "2");
	succeed_if_same_string (elektraKeyString (k4), "3");
	succeed_if_same_string (elektraKeyString (k5), "1");
	succeed_if_same_string (elektraKeyString (k6), "3");
	succeed_if_same_string (elektraKeyString (k7), "3");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (elektraKeyString (k1), "none");
	succeed_if_same_string (elektraKeyString (k2), "low");
	succeed_if_same_string (elektraKeyString (k3), "high");
	succeed_if_same_string (elektraKeyString (k4), "high_low");
	succeed_if_same_string (elektraKeyString (k5), "none_low");
	succeed_if_same_string (elektraKeyString (k6), "low_low_high");
	succeed_if_same_string (elektraKeyString (k7), "low_high");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if_same_string (elektraKeyString (k1), "0");
	succeed_if_same_string (elektraKeyString (k2), "1");
	succeed_if_same_string (elektraKeyString (k3), "2");
	succeed_if_same_string (elektraKeyString (k4), "3");
	succeed_if_same_string (elektraKeyString (k5), "1");
	succeed_if_same_string (elektraKeyString (k6), "3");
	succeed_if_same_string (elektraKeyString (k7), "3");

	elektraKeySetString (k1, "3");
	elektraKeySetString (k2, "0");
	elektraKeySetString (k3, "1");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	succeed_if_same_string (elektraKeyString (k1), "high_low");
	succeed_if_same_string (elektraKeyString (k2), "none");
	succeed_if_same_string (elektraKeyString (k3), "low");
	succeed_if_same_string (elektraKeyString (k4), "high_low");
	succeed_if_same_string (elektraKeyString (k5), "none_low");
	succeed_if_same_string (elektraKeyString (k6), "low_low_high");
	succeed_if_same_string (elektraKeyString (k7), "low_high");


	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_booleanDefault (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "true", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/t2", ELEKTRA_KEY_VALUE, "tRUe", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f1", ELEKTRA_KEY_VALUE, "false", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f2", ELEKTRA_KEY_VALUE, "falsE", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/off", ELEKTRA_KEY_VALUE, "off", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/on", ELEKTRA_KEY_VALUE, "on", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t2", 0)), "1");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f2", 0)), "0");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/on", 0)), "1");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/off", 0)), "0");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanDefaultRestore (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/a0", ELEKTRA_KEY_VALUE, "x", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "true", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/t2", ELEKTRA_KEY_VALUE, "tRUe", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f1", ELEKTRA_KEY_VALUE, "false", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f2", ELEKTRA_KEY_VALUE, "falsE", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/off", ELEKTRA_KEY_VALUE, "off", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/on", ELEKTRA_KEY_VALUE, "on", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t2", 0)), "tRUe");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanDefaultError (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "true", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/t2", ELEKTRA_KEY_VALUE, "tRUe", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/nt", ELEKTRA_KEY_VALUE, "i'm not true", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f1", ELEKTRA_KEY_VALUE, "false", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f2", ELEKTRA_KEY_VALUE, "falsE", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/nf", ELEKTRA_KEY_VALUE, "i'm not false", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/off", ELEKTRA_KEY_VALUE, "off", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/on", ELEKTRA_KEY_VALUE, "on", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet was not successful");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValue (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("user:/booleans", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#0/true", ELEKTRA_KEY_VALUE, "strangeTrueValue", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#0/false", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/true", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/false", ELEKTRA_KEY_VALUE, "strangeFalseValue", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "strangeTrueValue", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f1", ELEKTRA_KEY_VALUE, "strangeFalseValue", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "strangeTrueValue");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f1", 0)), "strangeFalseValue");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValueWeird (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("user:/booleans", ELEKTRA_KEY_VALUE, "#3", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#0/true", ELEKTRA_KEY_VALUE, "strangeTrueValue", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#0/false", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/true", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/false", ELEKTRA_KEY_VALUE, "strangeFalseValue", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "strangeTrueValue", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/type/f1", ELEKTRA_KEY_VALUE, "strangeFalseValue", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "strangeTrueValue");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/f1", 0)), "strangeFalseValue");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanUserValueError (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("user:/booleans", ELEKTRA_KEY_VALUE, "#1", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#0/true", ELEKTRA_KEY_VALUE, "strangeTrueValue", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/true", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/booleans/#1/false", ELEKTRA_KEY_VALUE, "strangeFalseValue", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen ("type", modules, conf, errorKey);
	succeed_if (plugin == NULL, "plugin open should have failed");
	succeed_if (elektraKeyGetMeta (errorKey, "error") != NULL, "no error found after failed open");
	succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (errorKey, "error/reason")),
				"You must set both true and false for a boolean pair (config key: '/booleans/#0')");
	elektraKeyDel (errorKey);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanChangeValue (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	elektraKeySetString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0), "yes");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "yes");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	elektraKeySetString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0), "1");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanOverride (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/boolean/restoreas", ELEKTRA_KEY_VALUE, "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "t", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_META, "check/boolean/true", "t", ELEKTRA_KEY_META,
			   "check/boolean/false", "f", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (30, k1, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	elektraKeySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	elektraKeySetString (k1, "t");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreAsDefault (const char * type, const char * restore, const char * restoreTrue, const char * restoreFalse)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/boolean/restoreas", ELEKTRA_KEY_VALUE, restore, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "enabled", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (30, k1, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), restoreTrue);

	elektraKeySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), restoreTrue);

	elektraKeySetString (k1, "false");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), restoreFalse);

	elektraKeySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), restoreFalse);

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreAs (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (4, elektraKeyNew ("user:/boolean/restoreas", ELEKTRA_KEY_VALUE, "#0", ELEKTRA_KEY_END), elektraKeyNew ("user:/booleans", ELEKTRA_KEY_VALUE, "#0", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/booleans/#0/true", ELEKTRA_KEY_VALUE, "t", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/booleans/#0/false", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "t", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (30, k1, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	elektraKeySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "t");

	elektraKeySetString (k1, "f");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "f");

	elektraKeySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "f");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_booleanRestoreDisabled (const char * type)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/type", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/boolean/restoreas", ELEKTRA_KEY_VALUE, "none", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("type");
	ElektraKey * k1 = elektraKeyNew ("user:/tests/type/t1", ELEKTRA_KEY_VALUE, "true", ELEKTRA_KEY_META, type, "boolean", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (30, k1, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	elektraKeySetString (k1, "1");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "1");

	elektraKeySetString (k1, "false");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	elektraKeySetString (k1, "0");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if_same_string (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/type/t1", 0)), "0");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

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
