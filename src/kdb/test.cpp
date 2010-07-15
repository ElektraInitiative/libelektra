#include <test.hpp>

#include <vector>
#include <iostream>

#include <kdb>

using namespace kdb;
using namespace std;

TestCommand::TestCommand():
	root(),
	nrTest(0),
	nrError(0)
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

		nrTest ++;
		if (!test.lookup(t))
		{
			nrError ++;
			cerr << "Basic test failed" << endl;
		}
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

			Key res = test.lookup(t);

			nrTest ++;
			if (!res)
			{
				nrError ++;
				cerr << "String test failed (key not found)" << t << endl;
				continue;
			}

			nrTest ++;
			if (res.getString() != teststrings[i])
			{
				nrError ++;
				cerr << "String test failed (value is not equal)" << endl;
				cerr << "We got: \"" << res.getString() << "\"" << endl;
				cerr << "We wanted: \"" <<  teststrings[i] << "\"" << endl;
			}
		}
	}
}

void TestCommand::doBinaryTest()
{
	vector<string> teststrings;
	teststrings.push_back("binary value");
	teststrings.push_back("binary value with null");
	teststrings.back().push_back('\0');

	teststrings.push_back("binary value with null");
	teststrings.back().push_back('\0');
	teststrings.back() += "in the middle";

	teststrings.push_back(" a very long value with many spaces and basically very very long, but only binary text ... ");
	for (int i=0; i<256; ++i)
	{
		teststrings.back().push_back('\0');
		teststrings.back() += " very very long, but only binary text ... ";
	}

	teststrings.push_back("all chars:");
	for (int i=0; i<256; ++i) teststrings.back().push_back(i);
	teststrings.push_back("€");
	for (int i=0; i<256; ++i)
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
			t.addBaseName ("binary");
			t.setBinary(teststrings[i].c_str(), teststrings[i].length());

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
			t.addBaseName ("binary");

			Key res = test.lookup(t);
			nrTest ++;
			if (!res)
			{
				nrError ++;
				cerr << "Binary test failed (key not found)" << t << endl;
				continue;
			}

			nrTest ++;
			if (res.getValueSize() != teststrings[i].length())
			{
				nrError ++;
				cerr << "Binary test failed (length is not equal)" << endl;
				cerr << "We got: \"" << res.getBinary() << "\"" << endl;
				cerr << "We wanted: \"" << teststrings[i] << "\"" << endl;
			}

			nrTest ++;
			if (res.getBinary() != teststrings[i])
			{
				nrError ++;
				cerr << "Binary test failed (value is not equal)" << endl;
				cerr << "We got: \"" << res.getBinary() << "\"" << endl;
				cerr << "We wanted: \"" << teststrings[i] << "\"" << endl;
			}
		}
	}
}

void TestCommand::doTests()
{
	doBasicTest();
	doStringTest();
	doBinaryTest();
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

	cerr << "We got " << nrError << " errors in " << nrTest << " testcases." << endl;

	cout << "Test suite is now finished." << endl;
	cout << "Now restoring the original keyset." << endl;
	kdb.set(original, root);

	return nrError;
}

TestCommand::~TestCommand()
{}
