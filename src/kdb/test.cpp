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
	{
		KDB kdb;
		Key t = root.dup();
		t.addBaseName ("basic");
		t.setString ("BasicString");
		KeySet basic;
		basic.append(t);

		KeySet test;
		kdb.get (test, root);
		kdb.set (basic, root);
	}

	{
		KDB kdb;
		Key t = root.dup();
		t.addBaseName ("basic");
		t.setString ("BasicString");

		KeySet test;
		kdb.get (test, root);

		if (!test.lookup(t)) cerr << "Basic test failed" << endl;
	}
}

void TestCommand::doStringTest()
{
	vector<string> teststrings;
	teststrings.push_back("");
	teststrings.push_back("value");
	teststrings.push_back("value with spaces");
	teststrings.push_back(" a very long value with many spaces and basically very very long, but only text ... ");
	for (int i=1; i<256; ++i) teststrings.back() += " very very long, but only text ... ";
	teststrings.push_back("ascii umlauts !\"§$%&/()=?`\\}][{");
	teststrings.push_back("utf8 umlauts ¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·");
	teststrings.push_back("all chars:");
	for (int i=1; i<256; ++i) teststrings.back().push_back(i);
	teststrings.push_back("€");
	for (int i=1; i<256; ++i)
	{
		string s;
		s.push_back (i);
		teststrings.push_back(s);
	}


	for (size_t i = 0; i< teststrings.size(); ++i)
	{
		{
			KDB kdb;
			Key t = root.dup();
			t.addBaseName ("string");
			t.setString (teststrings[i]);

			KeySet basic;
			basic.append(t);

			KeySet test;
			kdb.get (test, root);
			kdb.set (basic, root);
		}

		{
			KDB kdb;

			KeySet test;
			kdb.get (test, root);

			Key t = root.dup();
			t.addBaseName ("string");
			t.setString (teststrings[i]);

			Key res = test.lookup(t);
			if (!res)
			{
				cerr << "String test failed (key not found)" << t << endl;
				continue;
			}
			if (res.getString() != teststrings[i])
			{
				cerr << "String test failed (string not equal)" << endl;
				cerr << "We got: \"" << res << "\"" << endl;
				cerr << "We wanted: \"" << t  << "\"" << endl;
			}
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

	root.setName(argv[2]);
	if (root.getNameSize() <= 1)
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	KDB kdb;
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
