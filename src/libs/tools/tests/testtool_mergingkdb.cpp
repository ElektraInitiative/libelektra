/**
 * @file
 *
 * @brief Tests for MergingKDB
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <chrono>
#include <gtest/gtest-elektra.h>
#include <gtest/gtest.h>
#include <merging/automergeconfiguration.hpp>
#include <merging/mergingkdb.hpp>
#include <merging/threewaymerge.hpp>
#include <thread>


using namespace kdb;
using namespace tools::merging;

class MergingKDBTest : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string configFile;

	KDB first;

	KeySet firstReturned;
	KeySet secondReturned;

	Key parent;

	MergingKDB mergingKDB;
	ThreeWayMerge merger;

	testing::Namespaces namespaces;
	testing::MountpointPtr mp;

	MergingKDBTest () : parent (testRoot, KEY_END), mergingKDB (), namespaces ()
	{
		clearConfigFile ();
	}

	void clearConfigFile ()
	{
		KDB repo;
		KeySet ks;
		repo.get (ks, parent);
		ks.clear ();
		repo.set (ks, parent);
	}

	virtual void SetUp () override
	{
		mp.reset (new testing::Mountpoint (testRoot, configFile));
	}

	virtual void TearDown () override
	{
		mp.reset ();
	}
};

const std::string MergingKDBTest::configFile = "kdbFile.dump";
const std::string MergingKDBTest::testRoot = "/tests/merging/";

TEST_F (MergingKDBTest, HandlesUnconflictingKeySets)
{
	first.get (firstReturned, parent);
	firstReturned.append (Key ("system" + testRoot + "key", KEY_VALUE, "value", KEY_END));
	first.set (firstReturned, parent);

	mergingKDB.get (secondReturned, parent);
	secondReturned.append (Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	mergingKDB.synchronize (secondReturned, parent, merger);
}

TEST_F (MergingKDBTest, ThrowsIfNoConflictStrategyRegistered)
{
	first.get (firstReturned, parent);
	mergingKDB.get (secondReturned, parent);
	std::this_thread::sleep_for (std::chrono::milliseconds (100));

	firstReturned.append (Key ("system" + testRoot + "key", KEY_VALUE, "value", KEY_END));
	first.set (firstReturned, parent);

	secondReturned.append (Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	EXPECT_THROW (mergingKDB.synchronize (secondReturned, parent, merger), MergingKDBException);
}

TEST_F (MergingKDBTest, MergesResolvableConflicts)
{
	AutoMergeConfiguration configuration;
	configuration.configureMerger (merger);

	first.get (firstReturned, parent);
	mergingKDB.get (secondReturned, parent);
	std::this_thread::sleep_for (std::chrono::milliseconds (100));

	Key key1 ("system" + testRoot + "key1", KEY_VALUE, "value", KEY_END);
	firstReturned.append (key1);
	first.set (firstReturned, parent);

	Key key2 ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END);
	secondReturned.append (key2);
	mergingKDB.synchronize (secondReturned, parent, merger);

	first.get (firstReturned, parent);
	Key resultKey1 = firstReturned.lookup ("system" + testRoot + "key1");
	Key resultKey2 = firstReturned.lookup ("system" + testRoot + "key2");
	EXPECT_EQ (2, firstReturned.size ()) << "Written KeySet has a wrong size";
	EXPECT_EQ (key1, resultKey1) << "Key1 was not written correctly";
	EXPECT_EQ (key2, resultKey2) << "Key1 was not written correctly";
}
