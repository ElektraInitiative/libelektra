/**
 * @file
 *
 * @brief Tests for data type checker
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <tests.hpp>

#include "type_checker.hpp"

#include <locale>
#include <stdexcept>

using namespace elektra;

TEST(type, version)
{
	try {
		KeySet config;
		config.append(
			Key("system/require_version",
				KEY_VALUE, "3",
				KEY_END)
			);
		TypeChecker tc(config);
		succeed_if (false, "version should not match");
	}
	catch (const char *text)
	{
		succeed_if (true, "version should not match");
		succeed_if (!strcmp(text, "Required Version does not match 2"),
			"failed version text does not match");
	}

	try {
		KeySet config;
		config.append(
			Key("system/require_version",
				KEY_VALUE, "2",
				KEY_END)
			);
		TypeChecker tc(config);
		succeed_if (true, "version should match");
	}
	catch (const char *text)
	{
		succeed_if (false, "version should match");
	}
}

TEST(type, short)
{
	KeySet config;
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "short",
		KEY_END);
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-32768");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-32769");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("32768");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("32767");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("32767x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("32767 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty short");

	k.setString("0");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-32768");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-32769");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("32768");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("32767");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");
}

TEST(type, unsignedShort)
{
	KeySet config;
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_END);
	succeed_if (tc.check(k), "should check successfully");
	k.setString("0");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("32767x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("32767 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty unsigned_short");

	k.setString("0");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");
}

TEST(type, float)
{
	KeySet config;
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "float",
		KEY_END);
	succeed_if (tc.check(k), "should check successfully");
	k.setString("0");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("1.5");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("1,5");
	succeed_if (!tc.check(k), "should fail");
	try {
		std::locale::global(std::locale(""));
		succeed_if(1, "locale set correctly");
	}
	catch (std::runtime_error const & e)
	{
		std::cerr << "Could not set locale, the locales in your system are broken" << std::endl;
	}
	k.setString("1,5");
	succeed_if (!tc.check(k), "should fail even if global locale was changed");
	k.setString("1233322.5");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("123233223322333322.5");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("123233223322333322.0001");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString(".");
	succeed_if (!tc.check(k), "should fail");
	k.setString("1.");
	succeed_if (tc.check(k), "should check successfully");
	k.setString(".5");
	succeed_if (tc.check(k), "should check successfully");
}

TEST(type, bool)
{
	KeySet config;
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "boolean",
		KEY_END);
	succeed_if (tc.check(k), "should check successfully");
	k.setString("1");
	succeed_if (tc.check(k), "should check successfully");
	k.setString(".");
	succeed_if (!tc.check(k), "should fail");
	k.setString("true");
	succeed_if (!tc.check(k), "should fail");
	k.setString("false");
	succeed_if (!tc.check(k), "should fail");
}

TEST(type, none)
{
	KeySet config;
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "anything",
		KEY_END);
	succeed_if (tc.check(k), "should check successfully");
	k.setString("1");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("any other");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("do not care");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("");
	succeed_if (tc.check(k), "should check successfully");
}

TEST(type, enforce)
{
	KeySet config;
	// enforce key just needs to be present
	config.append(Key("system/enforce", KEY_END));
	TypeChecker tc(config);

	Key k ("user/anything",
		KEY_VALUE, "anything",
		KEY_END);
	succeed_if (!tc.check(k), "should check successfully");
	k.setString("1");
	succeed_if (!tc.check(k), "should check successfully");
	k.setString("any other");
	succeed_if (!tc.check(k), "should check successfully");
	k.setString("do not care");
	succeed_if (!tc.check(k), "should check successfully");
	k.setString("");
	succeed_if (!tc.check(k), "should check successfully");
}

TEST(type, min)
{
	KeySet config;
	TypeChecker tc(config);

// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/min", "23",
		KEY_END);
// clang-format on
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("0");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("22");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("23");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("24");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("65534");
	succeed_if (tc.check(k), "should check successfully");

	k.setString("32768");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("32767");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("32766");
	succeed_if (tc.check(k), "should check successfully");

	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("32767x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("32767 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty unsigned_short");

	k.setString("0");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");
}

void test_max()
{
	KeySet config;
	TypeChecker tc(config);

// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/max", "123",
		KEY_END);
// clang-format on
	succeed_if (tc.check(k), "should pass");
	k.setString("0");
	succeed_if (tc.check(k), "should pass");
	k.setString("122");
	succeed_if (tc.check(k), "should pass");
	k.setString("123");
	succeed_if (tc.check(k), "should pass");
	k.setString("124");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("125");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("280");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("24");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("32767x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("32767 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty unsigned_short");

	k.setString("444");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");
}

TEST(type, minmax)
{
	KeySet config;
	TypeChecker tc(config);

// clang-format off
	Key k ("user/anything",
		KEY_VALUE, "0",
		KEY_META, "check/type", "unsigned_short",
		KEY_META, "check/type/min", "23",
		KEY_META, "check/type/max", "123",
		KEY_END);
// clang-format on
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("0");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("22");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("23");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("24");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("20x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("20 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty unsigned_short");

	k.setString("0");
	succeed_if (!tc.check(k), "should fail because below min");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");

	k.setMeta<string>("check/type", "unsigned_short");

	k.setString("38");
	succeed_if (tc.check(k), "should pass");
	k.setString("122");
	succeed_if (tc.check(k), "should pass");
	k.setString("123");
	succeed_if (tc.check(k), "should pass");
	k.setString("124");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("125");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("280");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("24");
	succeed_if (tc.check(k), "should check successfully");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (!tc.check(k), "should fail");
	k.setString("44x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");
	k.setString("48 x");
	succeed_if (!tc.check(k), "should fail because of garbage afterwards");

	k.setMeta<string>("check/type", "empty unsigned_short");

	k.setString("444");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("-1");
	succeed_if (!tc.check(k), "should fail (number too low)");
	k.setString("65536");
	succeed_if (!tc.check(k), "should fail (number too high)");
	k.setString("65535");
	succeed_if (!tc.check(k), "should fail because above max");
	k.setString("x");
	succeed_if (!tc.check(k), "should fail");
	k.setString("");
	succeed_if (tc.check(k), "should succeed (empty value)");
}
