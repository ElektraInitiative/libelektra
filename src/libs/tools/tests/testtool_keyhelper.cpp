/**
 * @file
 *
 * @brief Tests for the key helper
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <helper/keyhelper.hpp>

using namespace kdb;
using namespace kdb::tools::helper;

TEST (RebasePath, RebasesCorrectlyWithValidArguments)
{
	Key target = Key ("user:/test/configold/subdir/k1", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	EXPECT_EQ ("user:/test/confignew/subdir/k1", rebasePath (target, oldParent, newParent));
}

TEST (RebasePath, RebasesCorrectlyWithCascadingParent)
{
	Key target = Key ("user:/test/configold/subdir/k1", KEY_END);
	Key oldParent = Key ("/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	EXPECT_EQ ("user:/test/confignew/subdir/k1", rebasePath (target, oldParent, newParent));
}

TEST (RebasePath, WorksForKeyOnSameLevel)
{
	Key target = Key ("user:/test/configold", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	EXPECT_EQ ("user:/test/confignew", rebasePath (target, oldParent, newParent));
}

TEST (RebasePath, ThrowsExceptionOnInvalidRebase)
{
	Key target = Key ("user:/test/k1", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	EXPECT_THROW (rebasePath (target, oldParent, newParent), InvalidRebaseException);
}

TEST (RebasePath, CalculatesPathCorrectlyWithCascadingTarget)
{
	Key target = Key ("/test/k1", KEY_END);
	Key oldParent = Key ("spec:/test", KEY_END);
	Key newParent = Key ("spec:/test", KEY_END);

	EXPECT_EQ ("spec:/test/k1", rebasePath (target, oldParent, newParent));
}

TEST (RebaseKey, RebasesCorrectlyWithValidArguments)
{
	Key target = Key ("user:/test/configold/subdir/k1", KEY_VALUE, "testvalue", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);
	Key expected = Key ("user:/test/confignew/subdir/k1", KEY_VALUE, "testvalue", KEY_END);

	Key result = rebaseKey (target, oldParent, newParent);

	EXPECT_EQ (expected.getName (), result.getName ());
	EXPECT_EQ (expected.getString (), result.getString ());
}

TEST (RebaseKey, ThrowsExceptionOnInvalidRebase)
{
	Key target = Key ("user:/test/k1", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	EXPECT_THROW (rebasePath (target, oldParent, newParent), InvalidRebaseException);
}

TEST (RebaseKey, CreatesCopy)
{
	Key target = Key ("user:/test/configold/subdir/k1", KEY_VALUE, "testvalue", KEY_END);
	Key oldParent = Key ("user:/test/configold", KEY_END);
	Key newParent = Key ("user:/test/confignew", KEY_END);

	Key result = rebaseKey (target, oldParent, newParent);
	target.setString ("newvalue");
	EXPECT_EQ ("testvalue", result.getString ());
}

TEST (RemoveNamespace, Basics)
{
	Key key ("user:/test/configold/subdir/k1", KEY_VALUE, "testvalue", KEY_END);
	removeNamespace (key);
	EXPECT_EQ (key, Key ("/test/configold/subdir/k1", KEY_END));
}

TEST (CommonKeyName, Key1)
{
	EXPECT_EQ (commonKeyName (Key ("system:/test/script/error/x", KEY_END), Key ("system:/test/script/x", KEY_END)),
		   Key ("system:/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("system:/test/script//x", KEY_END), Key ("system:/test/script/other//x", KEY_END)),
		   Key ("system:/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("user:/test/script//x", KEY_END), Key ("system:/test/script/other//x", KEY_END)),
		   Key ("/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("/test/script//x", KEY_END), Key ("system:/test/script/other//x", KEY_END)),
		   Key ("/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("/test/script//x", KEY_END), Key ("/test/script/other//x", KEY_END)), Key ("/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("system:/test/script//x", KEY_END), Key ("user:/test/script/other//x", KEY_END)),
		   Key ("/test/script", KEY_END));
	EXPECT_EQ (commonKeyName (Key ("user:/test/script//x", KEY_END), Key ("user:/test/script/other//x", KEY_END)),
		   Key ("user:/test/script", KEY_END));
}
