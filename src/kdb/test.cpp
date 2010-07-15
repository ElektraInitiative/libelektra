#include <test.hpp>

#include <vector>
#include <iostream>

#include <kdb>

using namespace kdb;
using namespace std;

TestCommand::TestCommand()
{}

void TestCommand::doBasicTest()
{
	Key t = root.dup();
	t.addBaseName ("basic");
	t.setString ("BasicString");
	KeySet basic;
	basic.append(t);

	kdb.set (basic, root);

	KeySet test;
	kdb.get (test, root);

	if (!test.lookup(t)) cerr << "Basic test failed" << endl;
}

void TestCommand::doStringTest()
{
	vector<string> teststrings;
	teststrings.push_back("");

	for (size_t i = 0; i< teststrings.size(); ++i)
	{
		Key t = root.dup();
		t.addBaseName ("string");
		t.setString (teststrings[i]);
		KeySet basic;
		basic.append(t);

		kdb.set (basic, root);

		KeySet test;
		kdb.get (test, root);

		Key res = test.lookup(t);
		if (!res)
		{
			cerr << "String test failed (key not found)" << endl;
			continue;
		}
		if (res.getString() != teststrings[i])
		{
			cerr << "String test failed (string not equal)" << endl;
		}

	}
}

void TestCommand::doTests()
{
	doBasicTest();
	doStringTest();
}

int TestCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a key name" << endl;
		cerr << "Usage: test <name>" << endl;
		return 1;
	}

	root  = Key (argv[2], KEY_END);
	if (!root)
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	KeySet original;
	kdb.get(original, root);
	original.rewind();

	doTests();

	cout << "Test suite is now finished." << endl;
	cout << "Now restoring the original keyset." << endl;
	kdb.set(original, root);
	printWarnings(root);

	return 0;
}

TestCommand::~TestCommand()
{}
