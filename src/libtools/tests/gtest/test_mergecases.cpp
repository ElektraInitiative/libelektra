#include <iostream>
#include <gtest/gtest.h>
#include <merging/threewaymerge.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace kdb::tools::merging;

class MergeTest: public ::testing::Test
{
protected:

	KeySet base;
	KeySet ours;
	KeySet theirs;
	KeySet expectedMerge;
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
		expectedMerge.append (mergeParent);
		expectedMerge.append (Key ("user/parentm/config/key1", KEY_VALUE, "value1", KEY_END));
		expectedMerge.append (Key ("user/parentm/config/key2", KEY_VALUE, "value2", KEY_END));
		expectedMerge.append (Key ("user/parentm/config/key3", KEY_VALUE, "value3", KEY_END));
		expectedMerge.append (Key ("user/parentm/config/key4", KEY_VALUE, "value4", KEY_END));
		expectedMerge.append (Key ("user/parentm/config/key5", KEY_VALUE, "value5", KEY_END));

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
		compareKeys (expectedMerge.at (0), merged.at (0));
		compareKeys (expectedMerge.at (1), merged.at (1));
		compareKeys (expectedMerge.at (2), merged.at (2));
		compareKeys (expectedMerge.at (3), merged.at (3));
		compareKeys (expectedMerge.at (4), merged.at (4));
	}

	virtual void compareAllExceptKey1(KeySet& merged)
	{
		compareKeys (expectedMerge.at (0), merged.at (0));
		compareKeys (expectedMerge.at (2), merged.at (1));
		compareKeys (expectedMerge.at (3), merged.at (2));
		compareKeys (expectedMerge.at (4), merged.at (3));
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

TEST_F(MergeTest, EqualKeySetsMerge)
{

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();

	EXPECT_EQ(5, merged.size ());
	compareAllKeys (merged);

}

TEST_F(MergeTest, SameDeletedKeyMerge)
{

	ours.lookup ("user/parento/config/key1", KDB_O_POP);
	theirs.lookup ("user/parentt/config/key1", KDB_O_POP);
	theirs.lookup ("user/parentt/config/key1", KDB_O_POP);

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();

	EXPECT_EQ(4, merged.size ());
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, AddEqualsKeyMerge)
{
	Key addedKey = Key ("user/parento/config/key5", KEY_VALUE, "value5", KEY_END);
	ours.append (addedKey);
	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(6, merged.size ());
	compareAllKeys (merged);
}

TEST_F(MergeTest, EqualsAddKeyMerge)
{
	Key addedKey = Key ("user/parentt/config/key5", KEY_VALUE, "value5", KEY_END);
	theirs.append (addedKey);
	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(6, merged.size ());
	compareAllKeys (merged);
}

TEST_F(MergeTest, DeleteModifyConflict)
{
	ours.lookup ("user/parento/config/key1", KDB_O_POP);
	theirs.lookup ("user/parentt/config/key1").setString ("modifiedvalue");

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_TRUE(result.hasConflicts()) << "No conflict detected although conflicts should exist";

	KeySet conflicts = result.getConflictSet ();
	ASSERT_EQ(1, conflicts.size())<< "Wrong number of conflicts";
	testConflictMeta (conflicts.at (0), DELETE, MODIFY);

	KeySet merged = result.getMergedKeys ();
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, ModifyDeleteConflict)
{
	ours.lookup ("user/parento/config/key1").setString ("modifiedvalue");
	theirs.lookup ("user/parentt/config/key1", KDB_O_POP);

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	ASSERT_TRUE(result.hasConflicts())<< "No conflict detected although conflicts should exist";

	KeySet conflicts = result.getConflictSet ();
	EXPECT_EQ(1, conflicts.size()) << "Wrong number of conflicts";
	testConflictMeta (conflicts.at (0), MODIFY, DELETE);

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(4, merged.size ());
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, DeleteEqualsMerges)
{
	ours.lookup ("user/parento/config/key1", KDB_O_POP);

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(4, merged.size ());

	/* key with index 1 should be deleted */
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, EqualsDeleteMerges)
{
	theirs.lookup ("user/parentt/config/key1", KDB_O_POP);

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(4, merged.size ());

	/* key with index 1 should be deleted */
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, EqualsModifyMerges)
{
	theirs.lookup ("user/parentt/config/key1").setString ("modifiedvalue");

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(5, merged.size ());

	/* key with index 1 should be deleted */
	compareKeys (expectedMerge.at (0), merged.at (0));

	EXPECT_EQ(expectedMerge.at (1), merged.at (1));
	EXPECT_EQ ("modifiedvalue", merged.at(1).getString()) << "Key " << merged.at (1) << "was not modified correctly";

	compareKeys (expectedMerge.at (2), merged.at (2));
	compareKeys (expectedMerge.at (3), merged.at (3));
	compareKeys (expectedMerge.at (4), merged.at (4));
}

TEST_F(MergeTest, ModifyEqualsMerges)
{
	ours.lookup ("user/parento/config/key1").setString ("modifiedvalue");

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ(5, merged.size ());

	/* key with index 1 should be deleted */
	compareKeys (expectedMerge.at (0), merged.at (0));

	EXPECT_EQ(expectedMerge.at (1), merged.at (1));
	EXPECT_EQ ("modifiedvalue", merged.at(1).getString()) << "Key " << merged.at (1) << "was not modified correctly";

	compareKeys (expectedMerge.at (2), merged.at (2));
	compareKeys (expectedMerge.at (3), merged.at (3));
	compareKeys (expectedMerge.at (4), merged.at (4));
}

TEST_F(MergeTest, SameModifyConflict)
{
	ours.lookup ("user/parento/config/key1").setString ("modifiedvalueours");
	theirs.lookup ("user/parentt/config/key1").setString ("modifiedvaluetheirs");

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	ASSERT_TRUE (result.hasConflicts())<< "No conflict detected although conflicts should exist";

	KeySet conflicts = result.getConflictSet ();
	EXPECT_EQ(1, conflicts.size ());
	testConflictMeta (conflicts.at (0), MODIFY, MODIFY);

	KeySet merged = result.getMergedKeys ();

	EXPECT_EQ(4, merged.size ());
	compareAllExceptKey1 (merged);
}

TEST_F(MergeTest, SameAddedEqualValueMerges)
{
	ours.append (Key ("user/parento/config/key5", KEY_VALUE, "newvalue", KEY_END));
	theirs.append (Key ("user/parentt/config/key5", KEY_VALUE, "newvalue", KEY_END));

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);
	EXPECT_FALSE(result.hasConflicts()) << "Invalid conflict detected";

	KeySet merged = result.getMergedKeys ();

	EXPECT_EQ(6, merged.size ());
	compareAllKeys (merged);

	compareKeys (Key ("user/parentm/config/key5", KEY_VALUE, "newvalue", KEY_END), merged.at (5));
}

TEST_F(MergeTest, SameAddedDifferentValueConflict)
{
	ours.append (Key ("user/parento/config/key5", KEY_VALUE, "newvalueours", KEY_END));
	theirs.append (Key ("user/parentt/config/key5", KEY_VALUE, "newvaluetheirs", KEY_END));

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);

	ASSERT_TRUE (result.hasConflicts())<< "No conflict detected although conflicts should exist";

	KeySet conflicts = result.getConflictSet ();
	EXPECT_EQ(1, conflicts.size ());
	testConflictMeta (conflicts.at (0), ADD, ADD);

	KeySet merged = result.getMergedKeys ();

	EXPECT_EQ(5, merged.size ());

	compareAllKeys (merged);
}

TEST_F(MergeTest, SameMetaKeyModifyConflict)
{
	ours.lookup ("user/parento/config/key1").setMeta<std::string> ("testmeta", "ourvalue");
	theirs.lookup ("user/parentt/config/key1").setMeta<std::string> ("testmeta", "theirvalue");

	MergeResult result = ThreeWayMerge::mergeKeySet (base, ours, theirs, mergeParent);

	ASSERT_TRUE (result.hasConflicts())<< "No conflict detected although conflicts should exist";
	KeySet conflicts = result.getConflictSet ();
	EXPECT_EQ(1, conflicts.size ());
	testConflictMeta (conflicts.at (0), META, META);
	KeySet merged = result.getMergedKeys ();
	compareAllExceptKey1 (merged);
}
