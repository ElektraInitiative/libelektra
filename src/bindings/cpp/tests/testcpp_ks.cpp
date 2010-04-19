#include <tests.h>

void test_ksnew()
{
	cout << "testing keyset new" << endl;

	KeySet ks1;

	KeySet ks2 (5,
		ckdb::keyNew ("user/key2", KEY_END),
		KS_END);

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	// ks3.toStream(stdout, 0);

	Key k1("user/key4/1", KEY_END);
	Key k2("user/key4/2", KEY_REMOVE, KEY_END);
	Key k3("user/key4/3", KEY_VALUE, "value", KEY_END);
	KeySet ks4 (5,
		*k1, // k1 will lose its key and pass it to keyset
		*k2,
		*k3,
		KS_END);
	// ks4.toStream(stdout, 0);

	Key k4("user/key5/1", KEY_END);
	Key k5("user/key5/2", KEY_REMOVE, KEY_END);
	Key k6("user/key5/3", KEY_VALUE, "value", KEY_END);
	KeySet ks5 (5,
		k4.dup(),
		k5.dup(),
		k6.dup(),
		KS_END);
	// ks5.toStream(stdout, 0);
	// k4, k5, k6 can still be used
}

void test_ksdup()
{
	cout << "testing ksdup" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet ks4 (ks3.dup());
	succeed_if (ks3.size() == 3, "size not correct");
	succeed_if (ks4.size() == 3, "size not correct");

	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

void test_kscopy()
{
	cout << "testing ksdup" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet ks4 (ks3);
	succeed_if (ks3.size() == 3, "size not correct");
	succeed_if (ks4.size() == 3, "size not correct");

	KeySet ks5;
	ks5.copy(ks4);
	succeed_if (ks4.size() == 3, "size not correct");
	succeed_if (ks5.size() == 3, "size not correct");

	ks5.clear();
	succeed_if (ks5.size() == 0, "size not correct");



	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

void test_iterate()
{
	cout << "testing iterate" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks3.rewind();

	Key k1 = ks3.next();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.head(), "first key not head key");
	Key k2 = ks3.next();
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");
	succeed_if (k2.needRemove(), "remove not set");
	Key k3 = ks3.next();
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");
	succeed_if (k3 == ks3.tail(), "last key not tail key");
	try {
		ks3.next();
		succeed_if (false, "Out of Range not thrown");
	} catch (KeySetOutOfRange) { }

	ks3.rewind();
	for (size_t i=0; i<ks3.size(); i++)
	{
		Key k = ks3.next();
		char str[] = "user/key3/X";

		str [10] = i+'1';
		succeed_if (k.getName() == str, "wrong keyname");
	}
}

void test_cursor()
{
	cout << "testing cursor" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	cursor_t cursorTest;

	ks3.rewind();
	for (size_t i=0; i<ks3.size(); i++)
	{
		Key k = ks3.next();
		if (i==0) cursorTest = ks3.getCursor();
	}

	ks3.setCursor (cursorTest);
	Key k1 = ks3.current();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.head(), "first key not head key");
}

void test_pop()
{
	cout << "testing iterate" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks3.rewind();

	Key k3 = ks3.pop();
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");
	Key k2 = ks3.pop();
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");
	succeed_if (k2.needRemove(), "remove not set");
	Key k1 = ks3.pop();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	try {
		ks3.pop();
		succeed_if (false, "Out of Range not catched");
	} catch (KeySetOutOfRange) { }

	KeySet ks4 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks4.rewind();
	for (int i=ks4.size()-1; i>0; i--)
	{
		Key k = ks4.pop();
		char str[] = "user/key3/X";

		str [10] = i+'1';
		succeed_if (k.getName() == str, str);
	}
}


void test_lookup()
{
	cout << "testing lookup" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	Key k1 = ks3.lookup("user/key3/1");
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");

	Key k3 = ks3.lookup("user/key3/3");
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");

	try {
		ks3.lookup("user/key3/2");
		succeed_if (false, "Not Found not thrown for removed key");
	} catch (KeySetNotFound) { }

	try {
		ks3.lookup("user/key3/4");
		succeed_if (false, "Not Found not thrown for not existing key");
	} catch (KeySetNotFound) { }
}

void test_append()
{
	cout << "testing keyset append" << endl;

	KeySet ks1;

	KeySet ks2 (5,
		ckdb::keyNew ("user/key2", KEY_END),
		KS_END);
	ks1.append (ks2);

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	ks2.append (ks3);
	ks1.append (ks3);
	ks3.append (ks2);

	Key k1("user/key4/1", KEY_END);
	Key k2("user/key4/2", KEY_REMOVE, KEY_END);
	Key k3("user/key4/3", KEY_VALUE, "value", KEY_END);
	ks1.append (k1); ks1.append (k2); ks1.append (k3);
	ks2.append (k1); ks2.append (k2); ks2.append (k3);
	ks3.append (k1); ks3.append (k2); ks3.append (k3);

	KeySet ks4 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_REMOVE, KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	// ks1.toStream();
	// ks2.toStream();
	// ks3.toStream();
}


int main()
{
	cout << "KEYSET CLASS TESTS" << endl;
	cout << "==================" << endl << endl;

	test_ksnew();
	test_ksdup();
	test_kscopy();
	test_iterate();
	test_cursor();
	test_pop();
	test_lookup();
	test_append();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
