/**
 * \file
 *
 * \brief Implements a helper class for merge related tests
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <iostream>
#include <keysetio.hpp>
#include <gtest/gtest.h>
#include <merging/threewaymerge.hpp>

using namespace kdb;
using namespace kdb::tools::merging;

class MergeTest: public ::testing::Test
{
protected:

	KeySet base;
	KeySet ours;
	KeySet theirs;
	KeySet mergeKeys;
	Key baseParent;
	Key ourParent;
	Key theirParent;
	Key mergeParent;

	MergeTest()
	{
		baseParent = Key ("user/parentb", KEY_END);
		base.append (baseParent);
		base.append (Key ("user/parentb/config/key1", KEY_VALUE, "value1", KEY_END));
		base.append (Key ("user/parentb/config/key2", KEY_VALUE, "value2", KEY_END));
		base.append (Key ("user/parentb/config/key3", KEY_VALUE, "value3", KEY_END));
		base.append (Key ("user/parentb/config/key4", KEY_VALUE, "value4", KEY_END));

		ourParent = Key ("user/parento", KEY_END);
		ours.append (ourParent);
		ours.append (Key ("user/parento/config/key1", KEY_VALUE, "value1", KEY_END));
		ours.append (Key ("user/parento/config/key2", KEY_VALUE, "value2", KEY_END));
		ours.append (Key ("user/parento/config/key3", KEY_VALUE, "value3", KEY_END));
		ours.append (Key ("user/parento/config/key4", KEY_VALUE, "value4", KEY_END));

		theirParent = Key ("user/parentt", KEY_END);
		theirs.append (theirParent);
		theirs.append (Key ("user/parentt/config/key1", KEY_VALUE, "value1", KEY_END));
		theirs.append (Key ("user/parentt/config/key2", KEY_VALUE, "value2", KEY_END));
		theirs.append (Key ("user/parentt/config/key3", KEY_VALUE, "value3", KEY_END));
		theirs.append (Key ("user/parentt/config/key4", KEY_VALUE, "value4", KEY_END));

		/* used as reference for comparing by index */
		mergeParent = Key ("user/parentm", KEY_END);
		mergeKeys.append (mergeParent);
		mergeKeys.append (Key ("user/parentm/config/key1", KEY_VALUE, "value1", KEY_END));
		mergeKeys.append (Key ("user/parentm/config/key2", KEY_VALUE, "value2", KEY_END));
		mergeKeys.append (Key ("user/parentm/config/key3", KEY_VALUE, "value3", KEY_END));
		mergeKeys.append (Key ("user/parentm/config/key4", KEY_VALUE, "value4", KEY_END));
	}

	virtual ~MergeTest()
	{
	}

	virtual void SetUp()
	{
	}

	virtual void TearDown()
	{
	}

	virtual void compareKeys(const Key& k1, const Key& k2)
	{
		EXPECT_EQ(k1, k2) << "keys have different names";
		EXPECT_EQ(k1.getString(), k2.getString()) << "keys have different values";
	}

	virtual void compareAllKeys(KeySet& merged)
	{
		compareKeys (mergeKeys.at (0), merged.at (0));
		compareKeys (mergeKeys.at (1), merged.at (1));
		compareKeys (mergeKeys.at (2), merged.at (2));
		compareKeys (mergeKeys.at (3), merged.at (3));
		compareKeys (mergeKeys.at (4), merged.at (4));
	}

	virtual void compareAllExceptKey1(KeySet& merged)
	{
		compareKeys (mergeKeys.at (0), merged.at (0));
		compareKeys (mergeKeys.at (2), merged.at (1));
		compareKeys (mergeKeys.at (3), merged.at (2));
		compareKeys (mergeKeys.at (4), merged.at (3));
	}

	virtual void testConflictMeta(const Key& key, ConflictOperation our, ConflictOperation their)
	{
		Key const ourConflict = key.getMeta<Key const> ("conflict/operation/our");
		EXPECT_TRUE(ourConflict) << "No conflict metakey for our operation present";
		ConflictOperation operation = MergeConflictOperation::getFromName (ourConflict.getString ());
		EXPECT_EQ(our, operation);

		Key const theirConflict = key.getMeta<Key const> ("conflict/operation/their");
		EXPECT_TRUE(theirConflict) << "No conflict metakey for their operation present";
		operation = MergeConflictOperation::getFromName (theirConflict.getString ());
		EXPECT_EQ(their, operation);
	}
};



