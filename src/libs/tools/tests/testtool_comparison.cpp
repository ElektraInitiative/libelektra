/**
 * @file
 *
 * @brief Tests for the comparison helper
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <helper/comparison.hpp>

using namespace kdb;
using namespace kdb::tools::helper;

TEST (KeyDataEqualTest, IsTrueForSameValuedKeys)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue", KEY_END);

	EXPECT_TRUE (keyDataEqual (k1, k2));
}

TEST (KeyDataEqualTest, IsFalseForDifferentValuedKeys)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue2", KEY_END);

	EXPECT_FALSE (keyDataEqual (k1, k2));
}

TEST (KeyDataEqualTest, IsFalseForDifferentTypedKeys)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue1", KEY_BINARY, KEY_END);

	EXPECT_FALSE (keyDataEqual (k1, k2));
}

TEST (KeyDataEqualTest, HandlesNullKeys)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_END);
	Key nk = KeySet ().lookup ("invalid key");

	EXPECT_FALSE (keyDataEqual (k1, nk));
	EXPECT_FALSE (keyDataEqual (nk, k1));
	EXPECT_FALSE (keyDataEqual (nk, nk));
}

TEST (KeyMetaEqual, IsTrueForEmptyMeta)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue2", KEY_END);

	EXPECT_TRUE (keyMetaEqual (k1, k2));
}

TEST (KeyMetaEqual, IsTrueForEqualMeta)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_META, "testmeta", "testmetavalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue1", KEY_META, "testmeta", "testmetavalue1", KEY_END);

	EXPECT_TRUE (keyMetaEqual (k1, k2));
}

TEST (KeyMetaEqual, IsFalseForDifferentMeta)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_META, "testmeta", "testmetavalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue1", KEY_META, "testmeta", "testmetavalue2", KEY_END);

	EXPECT_FALSE (keyMetaEqual (k1, k2));
}

TEST (KeyMetaEqual, IsFalseForMissingMeta)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_META, "testmeta", "testmetavalue1", KEY_END);
	Key k2 = Key ("user:/test/config/key2", KEY_VALUE, "keyvalue1", KEY_END);

	EXPECT_FALSE (keyMetaEqual (k1, k2));
	EXPECT_FALSE (keyMetaEqual (k2, k1));
}

TEST (KeyMetaEqual, HandlesNullKeys)
{
	Key k1 = Key ("user:/test/config/key1", KEY_VALUE, "keyvalue1", KEY_END);
	Key nk = KeySet ().lookup ("invalid key");

	EXPECT_FALSE (keyMetaEqual (k1, nk));
	EXPECT_FALSE (keyMetaEqual (nk, k1));
	EXPECT_FALSE (keyMetaEqual (nk, nk));
}
