#include <tests.h>

#include "checker.hpp"

#include <locale>

using namespace elektra;

void test_short()
{
	TypeChecker tc;

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

void test_unsigned_short()
{
	TypeChecker tc;

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

void test_float()
{
	TypeChecker tc;

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
	std::locale::global(std::locale(""));
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

void test_bool()
{
	TypeChecker tc;

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

int main()
{
	cout << "  TYPE  TESTS" << endl;
	cout << "===============" << endl << endl;

	test_short();
	test_unsigned_short();
	test_float();
	test_bool();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
