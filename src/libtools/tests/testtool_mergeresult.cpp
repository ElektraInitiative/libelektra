/**
 * \file
 *
 * \brief Tests for the Mergeresult class
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <iostream>
#include <string>
#include <kdb.hpp>
#include <gtest/gtest.h>
#include <merging/mergeresult.hpp>


using namespace std;
using namespace kdb;
using namespace kdb::tools::merging;

TEST(MergeResult, ResolveConflictDeletesConflictMeta)
{
	MergeResult result;
	Key conflictKey = Key ("user/test/config/key1", KEY_VALUE, "testvalue", KEY_META, "conflict/operation/our",
			"delete", KEY_META, "conflict/operation/their", "modify", KEY_META, "conflict/test", "testvalue", KEY_END);

	Key test = Key ("");

	result.resolveConflict (conflictKey);

	EXPECT_FALSE(conflictKey.getMeta<const Key> ("conflict/operation/our"));
	EXPECT_FALSE(conflictKey.getMeta<const Key> ("conflict/operation/their"));
	EXPECT_FALSE(conflictKey.getMeta<const Key> ("conflict/test"));
}

TEST(MergeResult, ResolveConflictIgnoresOtherMeta)
{
	MergeResult result;
	Key conflictKey = Key ("user/test/config/key1", KEY_VALUE, "testvalue", KEY_META, "order", "10", KEY_META,
			"noconflict/data", "testvalue", KEY_END);

	result.resolveConflict (conflictKey);

	EXPECT_EQ("10", conflictKey.getMeta<string> ("order"));
	EXPECT_EQ("testvalue", conflictKey.getMeta<string> ("noconflict/data"));
}

TEST(MergeResult, ResolveConflictRemovesKeyFromConflicts)
{
	Key conflictKey = Key ("user/test/config/key1", KEY_VALUE, "testvalue", KEY_END);
	KeySet conflicts;
	conflicts.append (conflictKey);
	KeySet merged;
	MergeResult result (conflicts, merged);

	result.resolveConflict (conflictKey);

	EXPECT_EQ(0, result.getConflictSet().size ());
}
