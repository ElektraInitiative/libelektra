/**
 * @file
 *
 * @brief Tests for the Mergeresult class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <iostream>
#include <kdb.hpp>
#include <merging/mergeresult.hpp>
#include <string>


using namespace std;
using namespace kdb;
using namespace kdb::tools::merging;

TEST (MergeResult, ResolveConflictDeletesConflictMeta)
{
	MergeResult result;
	Key conflictKey = Key ("user:/test/config/key1", KEY_VALUE, "testvalue", KEY_META, "conflict/operation/our", "delete", KEY_META,
			       "conflict/operation/their", "modify", KEY_META, "conflict/test", "testvalue", KEY_END);

	Key test = Key ("/", KEY_END);

	result.resolveConflict (conflictKey);

	EXPECT_FALSE (conflictKey.getMeta<const Key> ("conflict/operation/our"));
	EXPECT_FALSE (conflictKey.getMeta<const Key> ("conflict/operation/their"));
	EXPECT_FALSE (conflictKey.getMeta<const Key> ("conflict/test"));
}

TEST (MergeResult, ResolveConflictIgnoresOtherMeta)
{
	MergeResult result;
	Key conflictKey = Key ("user:/test/config/key1", KEY_VALUE, "testvalue", KEY_META, "order", "10", KEY_META, "noconflict/data",
			       "testvalue", KEY_END);

	result.resolveConflict (conflictKey);

	EXPECT_EQ ("10", conflictKey.getMeta<string> ("order"));
	EXPECT_EQ ("testvalue", conflictKey.getMeta<string> ("noconflict/data"));
}

TEST (MergeResult, ResolveConflictRemovesKeyFromConflicts)
{
	Key conflictKey = Key ("user:/test/config/key1", KEY_VALUE, "testvalue", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey);
	KeySet merged;
	MergeResult result (conflicts, merged);

	result.resolveConflict (conflictKey);

	EXPECT_EQ (0, result.getConflictSet ().size ());
}

TEST (MergeResult, HasConflictsWorks)
{
	Key conflictKey = Key ("user:/test/config/key1", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey);
	KeySet merged;
	MergeResult result (conflicts, merged);
	EXPECT_TRUE (result.hasConflicts ());
	conflicts = KeySet ();
	result = MergeResult (merged, conflicts);
	EXPECT_FALSE (result.hasConflicts ());
}

TEST (MergeResult, IsConflictWorks)
{
	Key conflictKey = Key ("user:/test/config/key1", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey);
	KeySet merged;
	MergeResult result (conflicts, merged);
	EXPECT_TRUE (result.isConflict (conflictKey));
	EXPECT_FALSE (result.isConflict (Key ("user:/test/config/key2", KEY_END)));
}

TEST (MergeResult, CountsResolvedKeysCorrectly)
{
	Key conflictKey1 = Key ("user:/test/config/key1", KEY_END);
	Key conflictKey2 = Key ("user:/test/config/key2", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey1);
	conflicts.append (conflictKey2);
	KeySet merged;
	MergeResult result (conflicts, merged);
	EXPECT_EQ (0, result.getNumberOfResolvedKeys ());
	result.resolveConflict (conflictKey1);
	result.resolveConflict (conflictKey2);
	EXPECT_EQ (2, result.getNumberOfResolvedKeys ());
}

TEST (MergeResult, CountsEqualKeysCorrectly)
{
	Key mergedKey1 = Key ("user:/test/config/key1", KEY_END);
	Key mergedKey2 = Key ("user:/test/config/key2", KEY_END);
	Key mergedKey3 = Key ("user:/test/config/key3", KEY_END);
	Key conflictKey1 = Key ("user:/test/config/key4", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey1);
	KeySet merged;
	merged.append (mergedKey1);
	merged.append (mergedKey2);
	MergeResult result (conflicts, merged);
	EXPECT_EQ (2, result.getNumberOfEqualKeys ()) << "Initially merged keys not counted";
	result.resolveConflict (conflictKey1);
	result.addMergeKey (conflictKey1);
	EXPECT_EQ (2, result.getNumberOfEqualKeys ()) << "Resolved key is counted as equal key";
	result.addMergeKey (mergedKey3);
	EXPECT_EQ (3, result.getNumberOfEqualKeys ()) << "Merged key is not counted as equal key";
}
