/**
 * @file
 *
 * @brief Tests for the MetaMergeStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./mergetestutils.cpp"
#include <gtest/gtest.h>
#include <merging/metamergestrategy.hpp>
#include <merging/onesidestrategy.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools::merging;

class MetaMergeStrategyTest : public MergeTest
{
protected:
	MergeResult result;
	MergeTask task;
	KeySet conflicts;

	MetaMergeStrategyTest ()
	: task (MergeTask (BaseMergeKeys (base, baseParent), OurMergeKeys (ours, ourParent), TheirMergeKeys (theirs, theirParent),
			   mergeParent))
	{
		result = MergeResult (conflicts, mergeKeys);
	}
};

TEST_F (MetaMergeStrategyTest, MergesMetaWithInnerStrategy)
{
	base.lookup ("user:/parentb/config/key1").setMeta ("testmeta", "valueb");
	ours.lookup ("user:/parento/config/key1").setMeta ("testmeta", "valueo");
	theirs.lookup ("user:/parentt/config/key1").setMeta ("testmeta", "valuet");
	Key conflictKey = mk1;
	result.addConflict (conflictKey, CONFLICT_META, CONFLICT_META);
	conflictKey = result.getConflictSet ().at (0);

	ThreeWayMerge merger;
	MergeConflictStrategy * strategy = new OneSideStrategy (OURS);
	merger.addConflictStrategy (strategy);
	MetaMergeStrategy metaStrategy (merger);
	metaStrategy.resolveConflict (task, conflictKey, result);
	delete (strategy);

	EXPECT_FALSE (result.hasConflicts ()) << "Invalid conflict detected";
	KeySet merged = result.getMergedKeys ();
	cout << merged << endl;
	EXPECT_EQ (4, merged.size ());

	EXPECT_EQ ("valueo", merged.lookup (mk1).getMeta<string> ("testmeta"));
}

// TODO: test conflict resolution
