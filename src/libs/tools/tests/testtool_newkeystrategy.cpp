/**
 * @file
 *
 * @brief Tests for the NewKeyStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./mergetestutils.cpp"
#include <gtest/gtest.h>
#include <merging/newkeystrategy.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools::merging;

class NewKeyStrategyTest : public MergeTest
{
protected:
	NewKeyStrategy strategy;
	MergeResult result;
	MergeTask task;
	KeySet conflicts;

	NewKeyStrategyTest ()
	: task (MergeTask (BaseMergeKeys (base, baseParent), OurMergeKeys (ours, ourParent), TheirMergeKeys (theirs, theirParent),
			   mergeParent))
	{
		result = MergeResult (conflicts, mergeKeys);
	}
};

TEST_F (NewKeyStrategyTest, AddEqualsKeyMerge)
{
	Key addedKey = Key ("user:/parento/config/key5", KEY_VALUE, "value5", KEY_END);
	task.ours.append (addedKey);
	mergeKeys.append (mk5);
	Key conflictKey = mergeKeys.lookup (mk5);
	result.addConflict (conflictKey, CONFLICT_ADD, CONFLICT_SAME);
	conflictKey = result.getConflictSet ().at (0);

	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ (5, merged.size ());
	compareAllKeys (merged);
}

TEST_F (NewKeyStrategyTest, AddEqualsRespectsBinaryData)
{
	Key addedKey = Key ("user:/parento/config/key5", KEY_BINARY, KEY_VALUE, "value5", KEY_END);
	task.ours.append (addedKey);
	Key conflictKey = mk5;
	result.addConflict (conflictKey, CONFLICT_ADD, CONFLICT_SAME);
	conflictKey = result.getConflictSet ().at (0);

	strategy.resolveConflict (task, conflictKey, result);
	EXPECT_TRUE (conflictKey.isBinary ());
}

TEST_F (NewKeyStrategyTest, EqualsAddKeyMerge)
{
	Key addedKey = Key ("user:/parentt/config/key5", KEY_VALUE, "value5", KEY_END);
	task.theirs.append (addedKey);
	mergeKeys.append (mk5);
	Key conflictKey = mergeKeys.lookup (mk5);
	result.addConflict (conflictKey, CONFLICT_SAME, CONFLICT_ADD);
	conflictKey = result.getConflictSet ().at (0);

	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	EXPECT_EQ (5, merged.size ());
	compareAllKeys (merged);
}

TEST_F (NewKeyStrategyTest, EqualsAddRespectsBinaryData)
{
	Key addedKey = Key ("user:/parentt/config/key5", KEY_BINARY, KEY_VALUE, "value5", KEY_END);
	task.theirs.append (addedKey);
	Key conflictKey = mk5;
	result.addConflict (conflictKey, CONFLICT_SAME, CONFLICT_ADD);
	conflictKey = result.getConflictSet ().at (0);

	strategy.resolveConflict (task, conflictKey, result);
	EXPECT_TRUE (conflictKey.isBinary ());
}
