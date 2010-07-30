#include <tests.h>

#include "checker.hpp"

using namespace elektra;

void test_basic()
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
}

int main()
{
	cout << "  TYPE  TESTS" << endl;
	cout << "===============" << endl << endl;

	test_basic();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
