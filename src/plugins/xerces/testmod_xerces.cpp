/**
 * @file
 *
 * @brief Tests for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <tests.h>
#include <tests.hpp>

#include "deserializer.hpp"

#include <iostream>

#include <xercesc/util/PlatformUtils.hpp>

TEST (xerces, simple)
{
	XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize ();

	KeySet ks;
	cout << "Starting with simple test" << endl;
	string file (srcdir_file ("testdata/simple.xml"));
	deserialize (file, ks);
	cout << "yes " << ks.size () << endl;
	succeed_if (ks.size (), "ks is empty after reading simple.xml");

	// test xerces key with value
	Key xercesKey = ks.lookup ("/sw/elektra/tests/xerces");
	succeed_if (xercesKey, "/sw/elektra/tests/xerces not parsed");
	cout << xercesKey.get<string> () << endl;
	succeed_if (xercesKey.get<string> () == "value of xerces", "xercesKey has wrong value");

	// test fizz key with attribute
	Key fizzKey = ks.lookup ("/sw/elektra/tests/xerces/fizz");
	succeed_if (fizzKey, "/sw/elektra/tests/xerces/fizz not parsed");
	succeed_if (fizzKey.getMeta<string> ("buzz") == "fizzBuzz", "attribute of fizz not found or wrong");

	succeed_if (0, "forced error so i see some output ;)");

	XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate ();
}

/*
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}*/