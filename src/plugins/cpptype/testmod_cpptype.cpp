/**
 * @file
 *
 * @brief Tests for data type checker
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <tests.hpp>

#include "type_checker.hpp"

// #include <tests_plugin.h> // not available for C++
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h> // currently needed for plugin handle (struct _Plugin)

#include <locale>
#include <stdexcept>

using namespace elektra;


TEST (cpptype, version)
{
	try
	{
		KeySet config;
		config.append (Key ("system/require_version", KEY_VALUE, "3", KEY_END));
		TypeChecker tc (config);
		succeed_if (false, "version should not match");
	}
	catch (const char * text)
	{
		succeed_if (true, "version should not match");
		succeed_if (!strcmp (text, "Required Version does not match 2"), "failed version text does not match");
	}

	try
	{
		KeySet config;
		config.append (Key ("system/require_version", KEY_VALUE, "2", KEY_END));
		TypeChecker tc (config);
		succeed_if (true, "version should match");
	}
	catch (const char * text)
	{
		succeed_if (false, "version should match");
	}
}

TEST (cpptype, validate)
{
	ckdb::Key * parentKey = ckdb::keyNew ("system/elektra/modules/cpptype", KEY_END);
	ckdb::KeySet * conf = ckdb::ksNew (0, KS_END);

	ckdb::KeySet * modules = ckdb::ksNew (0, KS_END);
	ckdb::elektraModulesInit (modules, 0);
	ckdb::Key * errorKey = ckdb::keyNew ("", KEY_END);
	ckdb::Plugin * plugin = ckdb::elektraPluginOpen ("cpptype", modules, conf, errorKey);
	ckdb::keyDel (errorKey);
	exit_if_fail (plugin != 0, "could not open cpptype plugin");

	ckdb::KeySet * ks = ckdb::ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ckdb::Key * key;
	key = ckdb::ksLookupByName (ks, "system/elektra/modules/cpptype/exports/validateKey", 0);
	exit_if_fail (key, "key not found");

	union
	{
		int (*f) (ckdb::Key *, ckdb::Key *);
		void * v;
	} conversation;

	ASSERT_TRUE (ckdb::keyGetBinary (key, &conversation.v, sizeof (conversation)) == sizeof (conversation));

	ckdb::Key * k = ckdb::keyNew ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
	EXPECT_TRUE (conversation.f (k, parentKey));
	EXPECT_FALSE (ckdb::keyGetMeta (parentKey, "error/number"));

	ckdb::keySetString (k, "a");
	EXPECT_FALSE (conversation.f (k, parentKey));
	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "error/number"));

	ckdb::keyDel (k);

	ckdb::ksDel (ks);
	ckdb::keyDel (parentKey);

	ckdb::elektraPluginClose (plugin, 0);
	ckdb::elektraModulesClose (modules, 0);
	ckdb::ksDel (modules);
}

TEST (type, short)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "short", KEY_END);
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-32768");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-32769");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("32768");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("32767");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("32767x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("32767 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty short");

	k.setString ("0");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-32768");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-32769");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("32768");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("32767");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");
}

TEST (cpptype, unsignedShort)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "unsigned_short", KEY_END);
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("0");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("32767x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("32767 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty unsigned_short");

	k.setString ("0");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");
}

TEST (cpptype, float)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "float", KEY_END);
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("0");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("1.5");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("1,5");
	succeed_if (!tc.check (k), "should fail");
	try
	{
		std::locale::global (std::locale (""));
		succeed_if (1, "locale set correctly");
	}
	catch (std::runtime_error const & e)
	{
		std::cerr << "Could not set locale, the locales in your system are broken" << std::endl;
	}
	k.setString ("1,5");
	succeed_if (!tc.check (k), "should fail even if global locale was changed");
	k.setString ("1233322.5");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("123233223322333322.5");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("123233223322333322.0001");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString (".");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("1.");
	succeed_if (tc.check (k), "should check successfully");
	k.setString (".5");
	succeed_if (tc.check (k), "should check successfully");
}

TEST (cpptype, bool)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "0", KEY_META, "check/type", "boolean", KEY_END);
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("1");
	succeed_if (tc.check (k), "should check successfully");
	k.setString (".");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("true");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("false");
	succeed_if (!tc.check (k), "should fail");
}

TEST (cpptype, none)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "anything", KEY_END);
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("1");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("any other");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("do not care");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("");
	succeed_if (tc.check (k), "should check successfully");
}

TEST (cpptype, enforce)
{
	KeySet config;
	// enforce key just needs to be present
	config.append (Key ("system/enforce", KEY_END));
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "anything", KEY_END);
	succeed_if (!tc.check (k), "should check successfully");
	k.setString ("1");
	succeed_if (!tc.check (k), "should check successfully");
	k.setString ("any other");
	succeed_if (!tc.check (k), "should check successfully");
	k.setString ("do not care");
	succeed_if (!tc.check (k), "should check successfully");
	k.setString ("");
	succeed_if (!tc.check (k), "should check successfully");
}

TEST (cpptype, min)
{
	KeySet config;
	TypeChecker tc (config);

	// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/min", "23",
		KEY_END);
	// clang-format on
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("0");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("22");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("23");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("24");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("65534");
	succeed_if (tc.check (k), "should check successfully");

	k.setString ("32768");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("32767");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("32766");
	succeed_if (tc.check (k), "should check successfully");

	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("32767x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("32767 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty unsigned_short");

	k.setString ("0");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");
}

void test_max ()
{
	KeySet config;
	TypeChecker tc (config);

	// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/max", "123",
		KEY_END);
	// clang-format on
	succeed_if (tc.check (k), "should pass");
	k.setString ("0");
	succeed_if (tc.check (k), "should pass");
	k.setString ("122");
	succeed_if (tc.check (k), "should pass");
	k.setString ("123");
	succeed_if (tc.check (k), "should pass");
	k.setString ("124");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("125");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("280");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("24");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("32767x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("32767 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty unsigned_short");

	k.setString ("444");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");
}

TEST (cpptype, minmax)
{
	KeySet config;
	TypeChecker tc (config);

	// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/min", "23",
		KEY_META, "check/type/max", "123",
		KEY_END);
	// clang-format on
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("0");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("22");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("23");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("24");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("20x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("20 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty unsigned_short");

	k.setString ("0");
	succeed_if (!tc.check (k), "should fail because below min");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");

	k.setMeta<string> ("check/type", "unsigned_short");

	k.setString ("38");
	succeed_if (tc.check (k), "should pass");
	k.setString ("122");
	succeed_if (tc.check (k), "should pass");
	k.setString ("123");
	succeed_if (tc.check (k), "should pass");
	k.setString ("124");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("125");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("280");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("24");
	succeed_if (tc.check (k), "should check successfully");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("44x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");
	k.setString ("48 x");
	succeed_if (!tc.check (k), "should fail because of garbage afterwards");

	k.setMeta<string> ("check/type", "empty unsigned_short");

	k.setString ("444");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("-1");
	succeed_if (!tc.check (k), "should fail (number too low)");
	k.setString ("65536");
	succeed_if (!tc.check (k), "should fail (number too high)");
	k.setString ("65535");
	succeed_if (!tc.check (k), "should fail because above max");
	k.setString ("x");
	succeed_if (!tc.check (k), "should fail");
	k.setString ("");
	succeed_if (tc.check (k), "should succeed (empty value)");
}

TEST (cpptype, char)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "a", KEY_META, "check/type", "char", KEY_END);
	EXPECT_TRUE (tc.check (k)) << "a should check successfully as char";
	k.setString ("b");
	EXPECT_TRUE (tc.check (k)) << "b should check successfully as char";
	k.setString (" ");
	EXPECT_TRUE (tc.check (k)) << "space should check successfully as char";
	k.setString ("");
	EXPECT_FALSE (tc.check (k)) << "empty string should not check successfully as char";
	k.setString ("ab");
	EXPECT_FALSE (tc.check (k)) << "two chars should not check successfully as char";

	for (int i = 1; i < 255; ++i)
	{
		char x[2];
		x[0] = i;
		x[1] = 0;
		k.setString (x);
		EXPECT_TRUE (tc.check (k)) << x << " should check successfully as char";
	}
}

TEST (cpptype, octet)
{
	KeySet config;
	TypeChecker tc (config);

	Key k ("user/anything", KEY_VALUE, "a", KEY_META, "check/type", "octet", KEY_END);
	EXPECT_TRUE (tc.check (k)) << "a should check successfully as octet";
	k.setString ("b");
	EXPECT_TRUE (tc.check (k)) << "b should check successfully as octet";
	k.setString (" ");
	EXPECT_TRUE (tc.check (k)) << "space should check successfully as octet";
	k.setString ("");
	EXPECT_FALSE (tc.check (k)) << "empty string should not check successfully as octet";
	k.setString ("ab");
	EXPECT_FALSE (tc.check (k)) << "two chars should not check successfully as octet";

	for (int i = 1; i < 255; ++i)
	{
		char x[2];
		x[0] = i;
		x[1] = 0;
		k.setString (x);
		EXPECT_TRUE (tc.check (k)) << x << " should check successfully as octet";
	}
}
