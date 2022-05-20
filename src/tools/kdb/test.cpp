/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <test.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace kdb;
using namespace std;

TestCommand::TestCommand ()
: root (), nrTest (0), nrError (0),
  // XXX add here the name if you add a test
  // (spaces at begin/end needed for check)
  testNames (" basic string umlauts binary naming meta ")
{
}

void TestCommand::doBasicTest ()
{
	{
		KDB kdb;
		Key t = root.dup ();
		t.addBaseName ("basic");
		t.setString ("BasicString");
		KeySet basic;
		basic.append (t);

		KeySet test;
		kdb.get (test, root);
		kdb.set (basic, root);
	}

	{
		KDB kdb;
		Key t = root.dup ();
		t.addBaseName ("basic");
		t.setString ("BasicString");

		KeySet test;
		kdb.get (test, root);

		nrTest++;
		if (!test.lookup (t))
		{
			nrError++;
			cerr << "Basic test failed" << endl;
		}
	}
}

void TestCommand::doStringTest ()
{
	vector<string> teststrings;
	teststrings.push_back ("");
	teststrings.push_back ("value");
	teststrings.push_back ("value with spaces");
	teststrings.push_back (" a very long value with many spaces and basically very very long, but only text ... ");
	for (int i = 1; i < 256; ++i)
		teststrings.back () += " very very long, but only text ... ";


	for (auto & teststring : teststrings)
	{
		{
			KDB kdb;
			Key t = root.dup ();
			t.addBaseName ("string");
			t.setString (teststring);

			KeySet basic;
			basic.append (t);

			KeySet test;
			kdb.get (test, root);
			kdb.set (basic, root);
		}

		{
			KDB kdb;

			KeySet test;
			kdb.get (test, root);

			Key t = root.dup ();
			t.addBaseName ("string");

			Key res = test.lookup (t);

			nrTest++;
			if (!res)
			{
				nrError++;
				cerr << "String test failed (key not found)" << t.getName () << endl;
				continue;
			}

			nrTest++;
			if (res.getString () != teststring)
			{
				nrError++;
				cerr << "String test failed (value is not equal)" << endl;
				cerr << "We got: \"" << res.getString () << "\"" << endl;
				cerr << "We wanted: \"" << teststring << "\"" << endl;
			}
		}
	}
}

void TestCommand::doUmlautsTest ()
{
	vector<string> teststrings;
	teststrings.push_back ("ascii umlauts !\"§$%&/()=?`\\}][{");
	teststrings.push_back ("utf8 umlauts ¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·");
	teststrings.push_back ("all chars:");
	for (int i = 1; i < 256; ++i)
		teststrings.back ().push_back (i);
	teststrings.push_back ("€");
	for (int i = 1; i < 256; ++i)
	{
		string s;
		s.push_back (i);
		teststrings.push_back (s);
	}


	for (auto & teststring : teststrings)
	{
		{
			KDB kdb;
			Key t = root.dup ();
			t.addBaseName ("string");
			t.setString (teststring);

			KeySet basic;
			basic.append (t);

			KeySet test;
			kdb.get (test, root);
			kdb.set (basic, root);
		}

		{
			KDB kdb;

			KeySet test;
			kdb.get (test, root);

			Key t = root.dup ();
			t.addBaseName ("string");

			Key res = test.lookup (t);

			nrTest++;
			if (!res)
			{
				nrError++;
				cerr << "String test failed (key not found)" << t.getName () << endl;
				continue;
			}

			nrTest++;
			if (res.getString () != teststring)
			{
				nrError++;
				cerr << "String test failed (value is not equal)" << endl;
				cerr << "We got: \"" << res.getString () << "\"" << endl;
				cerr << "We wanted: \"" << teststring << "\"" << endl;
			}
		}
	}
}

void TestCommand::doBinaryTest ()
{
	vector<string> teststrings;
	teststrings.push_back ("binary value");
	teststrings.push_back ("binary value with null");
	teststrings.back ().push_back ('\0');

	teststrings.push_back ("binary value with null");
	teststrings.back ().push_back ('\0');
	teststrings.back () += "in the middle";

	teststrings.push_back (" a very long value with many spaces and basically very very long, but only binary text ... ");
	for (int i = 0; i < 256; ++i)
	{
		teststrings.back ().push_back ('\0');
		teststrings.back () += " very very long, but only binary text ... ";
	}

	teststrings.push_back ("all chars:");
	for (int i = 0; i < 256; ++i)
		teststrings.back ().push_back (i);
	teststrings.push_back ("€");
	for (int i = 0; i < 256; ++i)
	{
		string s;
		s.push_back (i);
		teststrings.push_back (s);
	}


	for (auto & teststring : teststrings)
	{
		{
			KDB kdb;
			Key t = root.dup ();
			t.addBaseName ("binary");
			t.setBinary (teststring.c_str (), teststring.length ());

			KeySet basic;
			basic.append (t);

			KeySet test;
			kdb.get (test, root);
			kdb.set (basic, root);
		}

		{
			KDB kdb;

			KeySet test;
			kdb.get (test, root);

			Key t = root.dup ();
			t.addBaseName ("binary");

			Key res = test.lookup (t);
			nrTest++;
			if (!res)
			{
				nrError++;
				cerr << "Binary test failed (key not found)" << t.getName () << endl;
				continue;
			}

			nrTest++;
			if (res.getBinarySize () > 0 && static_cast<size_t> (res.getBinarySize ()) != teststring.length ())
			{
				nrError++;
				cerr << "Binary test failed (length is not equal)" << endl;
				cerr << "We got: \"" << res.getBinary () << "\"" << endl;
				cerr << "We wanted: \"" << teststring << "\"" << endl;
			}

			nrTest++;
			if (res.getBinary () != teststring)
			{
				nrError++;
				cerr << "Binary test failed (value is not equal)" << endl;
				cerr << "We got: \"" << res.getBinary () << "\"" << endl;
				cerr << "We wanted: \"" << teststring << "\"" << endl;
			}
		}
	}
}

void TestCommand::doNamingTest ()
{
	vector<string> teststrings;
	teststrings.push_back ("\\");
	teststrings.push_back ("keyname");
	teststrings.push_back ("deep/below/keyname");
	teststrings.push_back ("keyname with spaces");
	teststrings.push_back ("deep/belowkeyname with spaces");
	teststrings.push_back (" a very long value with many spaces and basically very very long, but only text ");
	for (int i = 1; i < 256; ++i)
		teststrings.back () += "/ very very long, but only text ... ";
	teststrings.push_back ("ascii umlauts !\"§$%&/()=?`\\}][{");
	teststrings.push_back ("utf8 umlauts ¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·");
	teststrings.push_back ("all chars:");
	for (int i = 1; i < 256; ++i)
		teststrings.back ().push_back (i);
	teststrings.push_back ("€");
	for (int i = 1; i < 256; ++i)
	{
		if (i == '.') continue;
		string s;
		s.push_back (i);
		teststrings.push_back (s);
	}


	for (auto & teststring : teststrings)
	{
		{
			KDB kdb;
			Key t = root.dup ();
			t.addBaseName (teststring);

			KeySet basic;
			basic.append (t);

			KeySet test;
			kdb.get (test, root);
			kdb.set (basic, root);
		}

		{
			KDB kdb;

			KeySet test;
			kdb.get (test, root);

			Key res = test.at (0);

			nrTest++;
			if (!res)
			{
				nrError++;
				cerr << "Naming test failed (no key in keyset)" << endl;
				continue;
			}

			nrTest++;
			Key cmp = root.dup ();
			cmp.addBaseName (teststring);
			if (res != cmp)
			{
				nrError++;
				cerr << "Naming test failed (name is not equal)" << endl;
				cerr << "We got: \"" << res.getName () << "\"" << endl;
				cerr << "We wanted: \"" << cmp.getName () << "\"" << endl;
			}
		}
	}
}

void TestCommand::doMetaTest ()
{
	vector<string> teststrings;
	teststrings.push_back ("");
	teststrings.push_back ("value");
	teststrings.push_back ("value with spaces");
	teststrings.push_back (" a very long value with many spaces and basically very very long, but only text ... ");
	for (int i = 1; i < 256; ++i)
		teststrings.back () += " very very long, but only text ... ";
	teststrings.push_back ("ascii umlauts !\"§$%&/()=?`\\}][{");
	teststrings.push_back ("utf8 umlauts ¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·");
	teststrings.push_back ("all chars:");
	for (int i = 1; i < 256; ++i)
		teststrings.back ().push_back (i);
	teststrings.push_back ("€");
	for (int i = 1; i < 256; ++i)
	{
		string s;
		s.push_back (i);
		teststrings.push_back (s);
	}

	vector<string> testnames;
	testnames.push_back ("keyname");
	testnames.push_back ("deep/below/keyname");
	testnames.push_back ("keyname with spaces");
	testnames.push_back ("deep/belowkeyname with spaces");
	testnames.push_back (" a very long value with many spaces and basically very very long, but only text ");
	for (int i = 1; i < 256; ++i)
		testnames.back () += "/ very very long, but only text ... ";
	testnames.push_back ("ascii umlauts !\"§$%&/()=?`\\}][{");
	testnames.push_back ("utf8 umlauts ¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·");
	testnames.push_back ("all chars:");
	for (int i = 1; i < 256; ++i)
		testnames.back ().push_back (i);
	testnames.push_back ("€");
	for (int i = 1; i < 256; ++i)
	{
		if (i == 46) continue; // ignore .
		string s;
		s.push_back (i);
		testnames.push_back (s);
	}


	for (auto & testname : testnames)
		for (auto & teststring : teststrings)
		{
			{
				KDB kdb;
				Key t = root.dup ();
				t.addBaseName (testname);
				t.setMeta<string> ("key", teststring);

				KeySet basic;
				basic.append (t);

				KeySet test;
				kdb.get (test, root);
				kdb.set (basic, root);
			}

			{
				KDB kdb;

				KeySet test;
				kdb.get (test, root);

				Key t = root.dup ();
				t.addBaseName (testname);
				Key res = test.lookup (t);

				nrTest++;
				if (!res)
				{
					nrError++;
					cerr << "Meta test failed (key not found)" << t.getName () << endl;
					continue;
				}

				std::string meta = res.getMeta<std::string> ("key");

				nrTest++;
				if (meta != teststring)
				{
					nrError++;
					cerr << "Meta test failed (name is not equal)" << endl;
					cerr << "We got: \"" << meta << "\"" << endl;
					cerr << "We wanted: \"" << teststring << "\"" << endl;
				}
			}
		}
}

namespace
{
bool checkArgument (std::vector<std::string> const & arguments, std::string testname)
{
	return arguments.size () == 1 || find (arguments.begin (), arguments.end (), testname) != arguments.end ();
}
} // namespace

void TestCommand::doTests (std::vector<std::string> const & arguments)
{
	if (checkArgument (arguments, "basic"))
	{
		cout << "Doing basic tests" << std::endl;
		doBasicTest ();
	}
	if (checkArgument (arguments, "string"))
	{
		cout << "Doing string tests" << std::endl;
		doStringTest ();
	}
	if (checkArgument (arguments, "umlauts"))
	{
		cout << "Doing umlauts tests" << std::endl;
		doUmlautsTest ();
	}
	if (checkArgument (arguments, "binary"))
	{
		cout << "Doing binary tests" << std::endl;
		doBinaryTest ();
	}
	if (checkArgument (arguments, "naming"))
	{
		cout << "Doing naming tests" << std::endl;
		doNamingTest ();
	}
	if (checkArgument (arguments, "meta"))
	{
		cout << "Doing meta tests" << std::endl;
		doMetaTest ();
	}
	// XXX add here a new test execution (as above)
}

int TestCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () < 1)
	{
		throw invalid_argument ("need at least one argument");
	}

	// do a basic check on every argument
	for (size_t i = 1; i < cl.arguments.size (); ++i)
	{
		string name = " ";
		name += cl.arguments[i];
		name += " ";
		if (testNames.find (name) == std::string::npos)
		{
			throw invalid_argument ("test name " + cl.arguments[i] + " does not exist in:" + testNames);
		}
	}

	printWarnings (cerr, root, cl.verbose, cl.debug);

	root = cl.createKey (0);

	KDB kdb;
	KeySet original;
	kdb.get (original, root);

	doTests (cl.arguments);

	cerr << "We got " << nrError << " errors in " << nrTest << " test cases." << endl;

	cout << "Test suite is now finished." << endl;
	cout << "Now restoring the original keyset." << endl;
	kdb.set (original, root);

	printWarnings (cerr, root, cl.verbose, cl.debug);

	return nrError;
}

TestCommand::~TestCommand ()
{
}
