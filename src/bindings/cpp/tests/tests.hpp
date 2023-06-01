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

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define succeed_if(x, y) EXPECT_TRUE (x) << y

#define exit_if_fail(expression, message) ASSERT_TRUE (expression) << message

#define succeed_if_same(x, y, message) EXPECT_EQ (x, y) << message

#define compare_keyset(keySet1, keySet2) EXPECT_TRUE (compareKeySet (keySet1, keySet2))

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This function adds the string representation of a key to an assertion result.
 *
 * @param stream This parameter specifies the assertion result to which this function adds a string representation of `key`.
 * @param key This parameter stores the key this function converts to an assertion result.
 *
 * @returns An assertion result containing the string representation of `key`
 */
testing::AssertionResult & operator<< (testing::AssertionResult & stream, kdb::Key & key)
{
	stream << key.getName () << ": “" << (key.isString () ? key.getString () : (key.getBinarySize () == 0 ? "NULL" : "BINARY")) << "”";

	ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		const kdb::Key & curMeta = ckdb::ksAtCursor (metaKeys, it);
		stream << ", " << curMeta.getName () << ": “" << curMeta.getString () << "”";
	}

	stream << endl;
	return stream;
}

/**
 * @brief This function adds the string representation of a key set to an assertion result.
 *
 * @param stream This parameter specifies the assertion result to which this function adds a string representation of `keys`.
 * @param key This parameter stores the key set this function converts to an assertion result.
 *
 * @returns An assertion result containing the string representation of `keys`
 */
testing::AssertionResult & operator<< (testing::AssertionResult & stream, kdb::KeySet & keys)
{
	for (auto key : keys)
	{
		stream << key;
	}
	return stream;
}

/**
 * @brief This function checks if the meta key set of two keys are equal.
 *
 * @param key1 This parameter stores the first meta key set this function uses for comparison.
 * @param key2 This parameter stores the second meta key set this function uses for comparison.
 *
 * @retval true if all meta keys of `key1` and `key2` are equal
 * @retval false otherwise
 */
bool isMetaDataEqual (kdb::Key & key1, kdb::Key & key2)
{
	ckdb::KeySet * metaKeys1 = ckdb::keyMeta (key1.getKey ());
	ckdb::KeySet * metaKeys2 = ckdb::keyMeta (key2.getKey ());

	if (ckdb::ksGetSize (metaKeys1) != ckdb::ksGetSize (metaKeys2)) return false;

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys1); ++it)
	{
		const kdb::Key & curMeta1 = ckdb::ksAtCursor (metaKeys1, it);
		const kdb::Key & curMeta2 = ckdb::ksAtCursor (metaKeys2, it);

		if (curMeta1.getName () != curMeta2.getName ()) return false;
		if (curMeta1.getString () != curMeta2.getString ()) return false;
	}

	return true;
}

/**
 * @brief This function checks if two keys are equal.
 *
 * @param key1 This parameter stores the first key this function compares.
 * @param key2 This parameter stores the second key this function compares.
 *
 * @retval true if all attributes of `key1` and `key2` are equal
 * @retval false otherwise
 */
bool isKeyEqual (kdb::Key & key1, kdb::Key & key2)
{
	if (key1.getName () != key2.getName ()) return false;
	if ((key1.isString () && key2.isBinary ()) || (key1.isBinary () && key2.isString ())) return false;
	if (key1.getBinarySize () != key2.getBinarySize ()) return false;
	if (memcmp (key1.getValue (), key2.getValue (), key1.getBinarySize ())) return false;
	return isMetaDataEqual (key1, key2);
}

/**
 * @brief This function checks if two key sets are equal.
 *
 * @param keys1 This parameter stores the first key set this function compares.
 * @param keys2 This parameter stores the second key set this function compares.
 *
 * @retval true if all keys of `keys1` and `keys2` are equal
 * @retval false otherwise
 */
bool isKeySetEqual (kdb::KeySet & keys1, kdb::KeySet & keys2)
{
	if (keys1.size () != keys2.size ()) return false;

	for (ssize_t it = 0; it < keys1.size (); ++it)
	{
		kdb::Key key1 = keys1.at (it);
		kdb::Key key2 = keys2.at (it);

		if (!key1 || !key2) return false; // should never happen
		if (!isKeyEqual (key1, key2)) return false;
	}

	return true;
}

/**
 * @brief This function compares the given key sets.
 *
 * @param keys1 This parameter stores the first key set this function compares.
 * @param keys2 This parameter stores the second key set this function compares.
 *
 * @retval AssertionSuccess if all keys of `keys1` and `keys2` are equal
 * @retval AssertionFailure otherwise
 */
testing::AssertionResult compareKeySet (kdb::KeySet & keys1, kdb::KeySet & keys2)
{
	if (isKeySetEqual (keys1, keys2))
		return testing::AssertionSuccess ();
	else
		return testing::AssertionFailure () << "\n\nFirst key set:\n\n"
						    << keys1 << "\n——————————"
						    << "\n\nSecond key set:\n\n"
						    << keys2 << endl;
}

#endif
