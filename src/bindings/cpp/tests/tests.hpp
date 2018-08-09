/**
 * @file
 *
 * @brief Some common functions in use for testing framework
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDB_TESTS_HPP
#define KDB_TESTS_HPP

#include <kdb.hpp>
#include <kdbmacros.h>
#include <key.hpp>
#include <keyset.hpp>

#include <cstdlib>
#include <cstring>
#include <exception>
#include <iostream>
#include <string>

#include <gtest/gtest.h>

using namespace std;
using namespace kdb;

#define succeed_if(x, y) ASSERT_TRUE (x) << y

#define exit_if_fail(expression, message)                                                                                                  \
	if (!(expression))                                                                                                                 \
	{                                                                                                                                  \
		cerr << __FILE__ << ":" << __LINE__ << ": Failure" << endl;                                                                \
		cerr << "Value of: " << ELEKTRA_STRINGIFY (expression) << endl;                                                            \
		cerr << "  Actual: false" << endl;                                                                                         \
		cerr << "Expected: true" << endl;                                                                                          \
		cerr << message << endl;                                                                                                   \
		exit (1);                                                                                                                  \
	}                                                                                                                                  \
	SUCCEED () << message

#define succeed_if_same(x, y, message) ASSERT_EQ (x, y) << message

#define compare_keyset(keySet1, keySet2) ASSERT_TRUE (compareKeySet (keySet1, keySet2))

testing::AssertionResult & operator<< (testing::AssertionResult & stream, kdb::Key & key)
{
	stream << key.getName () << ": " << (key.isString () ? key.getString () : "binary value!") << endl;
	key.rewindMeta ();
	if (!key.nextMeta ()) return stream;

	stream << "Meta Keys: " << endl;
	key.rewindMeta ();
	while (key.nextMeta ())
	{
		stream << "\t" << key.currentMeta ().getName () << ": " << key.currentMeta ().getString () << endl;
	}
	return stream;
}

testing::AssertionResult & operator<< (testing::AssertionResult & stream, kdb::KeySet & keys)
{
	for (auto key : keys)
	{
		stream << key;
	}
	return stream;
}

bool isMetaDataEqual (kdb::Key & key1, kdb::Key & key2)
{
	key1.rewindMeta ();
	key2.rewindMeta ();

	while (key1.nextMeta ())
	{
		key2.nextMeta ();
		if (key1.currentMeta ().getName () != key2.currentMeta ().getName ()) return false;
		if (key1.currentMeta ().getString () != key2.currentMeta ().getString ()) return false;
	}

	return key1.nextMeta () == key2.nextMeta ();
}

bool isKeyEqual (kdb::Key & key1, kdb::Key & key2)
{
	if (key1.getName () != key2.getName ()) return false;
	if ((key1.isString () && key2.isBinary ()) || (key1.isBinary () && key2.isString ())) return false;
	if (key1.getBinarySize () != key2.getBinarySize ()) return false;
	if (memcmp (key1.getValue (), key2.getValue (), key1.getBinarySize ())) return false;
	return isMetaDataEqual (key1, key2);
}

bool isKeySetEqual (kdb::KeySet & keys1, kdb::KeySet & keys2)
{
	if (keys1.size () != keys2.size ()) return false;

	keys1.rewind ();
	keys2.rewind ();
	while (keys1.next ())
	{
		keys2.next ();
		kdb::Key key1 = keys1.current ();
		kdb::Key key2 = keys2.current ();
		if (!isKeyEqual (key1, key2)) return false;
	}

	return true;
}

testing::AssertionResult compareKeySet (kdb::KeySet & keys1, kdb::KeySet & keys2)
{
	if (isKeySetEqual (keys1, keys2))
		return testing::AssertionSuccess ();
	else
		return testing::AssertionFailure () << endl
						    << endl
						    << "First key set:" << endl
						    << endl
						    << keys1 << endl
						    << "——————————" << endl
						    << endl
						    << "Second key set:" << endl
						    << endl
						    << keys2 << endl;
}

#endif
