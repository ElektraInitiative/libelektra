#include <tests.hpp>

void test_kdbGetSet()
{
	cout << "testing kdbSet() and kdbGet()" << endl;

	KeySet ks_set (5,
		*Key ("user/tests/key3", KEY_DIR, KEY_END),
		*Key ("user/tests/key3/1", KEY_END),
		*Key ("user/tests/key3/2", KEY_END),
		*Key ("user/tests/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	KDB kdb;
	kdb.set (ks_set, Key("user/tests/key3", KEY_END));

	/*
	Key nonexist ("user/this/key/shuld/really/not/exist",KEY_END);
	bool ok = false;
	try {
		kdb.get (nonexist);
	} catch (KDBException) {
		ok = true;
	}
	succeed_if (ok, "get did not throw KDBException");
	*/

	KeySet ks_get;
	kdb.get (ks_get, Key("user/tests/key3", KEY_END));
	// ks_get.toStream();
	// ks_set.generate();
	
	// now remove keys
	kdb.set (ks_set, Key("user/tests", KEY_END), KDB_O_REMOVEONLY);
}

#include <cstdlib>

int main()
{
	cout << "KDB CLASS TESTS" << endl;
	cout << "==================" << endl << endl;

#ifdef HAVE_CLEARENV
	clearenv();
#endif
#ifdef HAVE_SETENV
	setenv ("KDB_HOME",".",1);
#endif

	test_kdbGetSet();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
