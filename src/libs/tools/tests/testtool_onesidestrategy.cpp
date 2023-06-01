/**
 * @file
 *
 * @brief Tests for the OneSideStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./mergetestutils.cpp"
#include <gtest/gtest.h>
#include <merging/onesidestrategy.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools::merging;

class OneSideStrategyTest : public MergeTest
{
protected:
	MergeResult result;
	MergeTask task;
	KeySet conflicts;

	OneSideStrategyTest ()
	: task (MergeTask (BaseMergeKeys (base, baseParent), OurMergeKeys (ours, ourParent), TheirMergeKeys (theirs, theirParent),
			   mergeParent))
	{
		result = MergeResult (conflicts, mergeKeys);
	}
};

TEST_F (OneSideStrategyTest, BaseWinsCorrectly)
{
	base.lookup ("user:/parentb/config/key1").setString ("valueb");
	ours.lookup ("user:/parento/config/key1").setString ("valueo");
	theirs.lookup ("user:/parentt/config/key1").setString ("valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (BASE);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	cout << merged << endl;
	EXPECT_EQ (4, merged.size ());

	compareKeys (Key ("user:/parentm/config/key1", KEY_VALUE, "valueb", KEY_END), merged.lookup (mk1));
}

TEST_F (OneSideStrategyTest, BaseWinnerRespectsBinaryData)
{
	base.lookup ("user:/parentb/config/key1").setBinary ("valueb", 6);
	ours.lookup ("user:/parento/config/key1").setString ("valueo");
	theirs.lookup ("user:/parentt/config/key1").setString ("valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (BASE);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_TRUE (conflictKey.isBinary ());
}

TEST_F (OneSideStrategyTest, OursWinsCorrectly)
{
	base.lookup ("user:/parentb/config/key1").setString ("valueb");
	ours.lookup ("user:/parento/config/key1").setString ("valueo");
	theirs.lookup ("user:/parentt/config/key1").setString ("valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (OURS);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	cout << merged << endl;
	EXPECT_EQ (4, merged.size ());

	compareKeys (Key ("user:/parentm/config/key1", KEY_VALUE, "valueo", KEY_END), merged.lookup (mk1));
}

TEST_F (OneSideStrategyTest, OursWinnerRespectsBinaryData)
{
	base.lookup ("user:/parentb/config/key1").setString ("valueb");
	ours.lookup ("user:/parento/config/key1").setBinary ("valueo", 6);
	theirs.lookup ("user:/parentt/config/key1").setString ("valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (OURS);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_TRUE (conflictKey.isBinary ());
}

TEST_F (OneSideStrategyTest, TheirsWinsCorrectly)
{
	base.lookup ("user:/parentb/config/key1").setString ("valueb");
	ours.lookup ("user:/parento/config/key1").setString ("valueo");
	theirs.lookup ("user:/parentt/config/key1").setString ("valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (THEIRS);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	cout << merged << endl;
	EXPECT_EQ (4, merged.size ());

	compareKeys (Key ("user:/parentm/config/key1", KEY_VALUE, "valuet", KEY_END), merged.lookup (mk1));
}

TEST_F (OneSideStrategyTest, TheirsWinnerRespectsBinaryData)
{
	base.lookup ("user:/parentb/config/key1").setString ("valueb");
	ours.lookup ("user:/parento/config/key1").setString ("valueo");
	theirs.lookup ("user:/parentt/config/key1").setBinary ("valuet", 6);
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
	conflictKey = result.getConflictSet ().at (0);

	OneSideStrategy strategy (THEIRS);
	strategy.resolveConflict (task, conflictKey, result);

	EXPECT_TRUE (conflictKey.isBinary ());
}
