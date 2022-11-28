/**
 * @file
 *
 * @brief Implements a helper class for merge related tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <iostream>
#include <kdbprivate.h>
#include <keysetio.hpp>
#include <merging/threewaymerge.hpp>

using namespace kdb;
using namespace kdb::tools::merging;

class MergeTest : public ::testing::Test
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
	Key mk1, mk2, mk3, mk4, mk5;

	MergeTest ()
	: baseParent (Key ("user:/parentb", KEY_END)), ourParent (Key ("user:/parento", KEY_END)),
	  theirParent (Key ("user:/parentt", KEY_END)), mergeParent (Key ("user:/parentm", KEY_END))
	{
		base.append (baseParent);
		base.append (Key ("user:/parentb/config/key1", KEY_VALUE, "value1", KEY_END));
		base.append (Key ("user:/parentb/config/key2", KEY_VALUE, "value2", KEY_END));
		base.append (Key ("user:/parentb/config/key3", KEY_VALUE, "value3", KEY_END));
		base.append (Key ("user:/parentb/config/key4", KEY_VALUE, "value4", KEY_END));

		ours.append (ourParent);
		ours.append (Key ("user:/parento/config/key1", KEY_VALUE, "value1", KEY_END));
		ours.append (Key ("user:/parento/config/key2", KEY_VALUE, "value2", KEY_END));
		ours.append (Key ("user:/parento/config/key3", KEY_VALUE, "value3", KEY_END));
		ours.append (Key ("user:/parento/config/key4", KEY_VALUE, "value4", KEY_END));

		theirs.append (theirParent);
		theirs.append (Key ("user:/parentt/config/key1", KEY_VALUE, "value1", KEY_END));
		theirs.append (Key ("user:/parentt/config/key2", KEY_VALUE, "value2", KEY_END));
		theirs.append (Key ("user:/parentt/config/key3", KEY_VALUE, "value3", KEY_END));
		theirs.append (Key ("user:/parentt/config/key4", KEY_VALUE, "value4", KEY_END));

		// used as references for comparing
		mk1 = Key ("user:/parentm/config/key1", KEY_VALUE, "value1", KEY_END);
		mk2 = Key ("user:/parentm/config/key2", KEY_VALUE, "value2", KEY_END);
		mk3 = Key ("user:/parentm/config/key3", KEY_VALUE, "value3", KEY_END);
		mk4 = Key ("user:/parentm/config/key4", KEY_VALUE, "value4", KEY_END);

		// used only by some tests
		mk5 = Key ("user:/parentm/config/key5", KEY_VALUE, "value5", KEY_END);

		mergeKeys.append (mk1);
		mergeKeys.append (mk2);
		mergeKeys.append (mk3);
		mergeKeys.append (mk4);
	}

	virtual ~MergeTest ()
	{
	}

	virtual void SetUp () override
	{
	}

	virtual void TearDown () override
	{
	}

	virtual void unsyncKeys (KeySet & ks)
	{
		for (const Key & current : ks)
		{
			current.getKey ()->needsSync = false;
		}
	}

	virtual void compareKeys (const Key & k1, const Key & k2)
	{
		EXPECT_EQ (k1, k2) << "keys have different names";
		EXPECT_EQ (k1.getString (), k2.getString ()) << "keys have different values";
	}

	virtual void compareAllKeys (KeySet & merged)
	{
		compareKeys (mk1, merged.lookup (mk1));
		compareKeys (mk2, merged.lookup (mk2));
		compareKeys (mk3, merged.lookup (mk3));
		compareKeys (mk4, merged.lookup (mk4));
	}

	virtual void compareAllExceptKey1 (KeySet & merged)
	{
		compareKeys (mk2, merged.lookup (mk2));
		compareKeys (mk3, merged.lookup (mk3));
		compareKeys (mk4, merged.lookup (mk4));
	}

	virtual void testConflictMeta (const Key & key, ConflictOperation our, ConflictOperation their)
	{
		Key const ourConflict = key.getMeta<Key const> ("conflict/operation/our");
		EXPECT_TRUE (ourConflict) << "No conflict metakey for our operation present";
		ConflictOperation operation = MergeConflictOperation::getFromName (ourConflict.getString ());
		EXPECT_EQ (our, operation);

		Key const theirConflict = key.getMeta<Key const> ("conflict/operation/their");
		EXPECT_TRUE (theirConflict) << "No conflict metakey for their operation present";
		operation = MergeConflictOperation::getFromName (theirConflict.getString ());
		EXPECT_EQ (their, operation);
	}
};
