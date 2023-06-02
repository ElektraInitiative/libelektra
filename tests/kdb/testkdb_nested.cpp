/**
 * @file
 *
 * @brief Tests for KDB
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <keysetio.hpp>

#include <gtest/gtest-elektra.h>


class Nested : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string testBelow;
	static const std::string configFileRoot;
	static const std::string configFileBelow;


	testing::Namespaces namespaces;
	testing::MountpointPtr mpRoot;
	testing::MountpointPtr mpBelow;

	Nested () : namespaces ()
	{
	}

	virtual void SetUp () override
	{
		mpRoot.reset (new testing::Mountpoint (testRoot, configFileRoot));
		mpBelow.reset (new testing::Mountpoint (testBelow, configFileBelow));
	}

	virtual void TearDown () override
	{
		mpBelow.reset ();
		mpRoot.reset ();
	}
};

const std::string Nested::testRoot = "/tests/kdb/";
const std::string Nested::testBelow = "/tests/kdb/below/";
const std::string Nested::configFileRoot = "kdbFileRoot.dump";
const std::string Nested::configFileBelow = "kdbFileBelow.dump";


TEST_F (Nested, GetSetNothing)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (kdb.set (ks, testRoot), 0) << "should be nothing to set";
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
}


TEST_F (Nested, GetSetRoot)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	std::string name = "system:" + testRoot + "key";
	Key k (name, KEY_END);
	EXPECT_EQ (k.getName (), name);
	ks.append (k);

	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 1) << "did not keep key at get" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (kdb.set (ks, testRoot), 1);
	ASSERT_EQ (ks.size (), 1) << "did not keep key at set" << ks;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), 0) << "root file not created";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";

	Key parent (testRoot, KEY_END);
	kdb.close (parent);
	kdb.open (parent);
	KeySet ks2;
	ASSERT_EQ (kdb.get (ks2, testRoot), 1);
	ASSERT_EQ (ks2.size (), 1) << "did not get key stored before" << ks;
}

kdb::KeySet getAll ()
{
	using namespace ckdb;
	return
#include "../data/data_allns.c"
}


TEST_F (Nested, GetSetBelow)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	std::string name = "system:" + testRoot + "below/key";
	Key k (name, KEY_END);
	EXPECT_EQ (k.getName (), name);
	ks.append (k);
	ks.append (getAll ());

	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 817) << "did not keep key at get" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (kdb.set (ks, testRoot), 1);
	ASSERT_EQ (ks.size (), 817) << "did not keep key at set" << ks;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "root file created?";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), 0) << "below file not created";

	Key parent (testRoot, KEY_END);
	kdb.close (parent);
	kdb.open (parent);
	KeySet ks2;
	ASSERT_EQ (kdb.get (ks2, testRoot), 1);
	ASSERT_EQ (ks2.size (), 1) << "did not get key stored before" << ks;
}


TEST_F (Nested, RemoveFiles)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system:" + testRoot + "key", KEY_END));
	ks.append (Key ("system:" + testRoot + "below/key", KEY_END));

	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 2) << "did not keep key at get" << ks;
	ASSERT_EQ (kdb.set (ks, testRoot), 1);
	ASSERT_EQ (ks.size (), 2) << "did not keep key at set" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), 0) << "root file not created";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), 0) << "below file not created";

	Key parent (testRoot, KEY_END);
	kdb.close (parent);
	kdb.open (parent);
	KeySet ks2;
	ASSERT_EQ (kdb.get (ks2, testRoot), 1);
	ASSERT_EQ (ks2.size (), 2) << "did not get key stored before" << ks;
	ks2.clear ();
	ASSERT_EQ (kdb.set (ks2, testRoot), 1); // remove files
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file, file not removed";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file, file not removed";
}


TEST_F (Nested, GetSetRemoveBoth)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system:" + testRoot + "key", KEY_END));
	ks.append (Key ("system:" + testRoot + "key/subkey", KEY_END));
	ks.append (Key ("system:" + testRoot + "below/key", KEY_END));
	ks.append (Key ("system:" + testRoot + "below/key/subkey", KEY_END));
	ks.append (getAll ());

	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 820) << "did not keep key at get" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (kdb.set (ks, testRoot), 1);
	ASSERT_EQ (ks.size (), 820) << "did not keep key at set" << ks;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), 0) << "root file not created";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), 0) << "below file not created";

	Key parent (testRoot, KEY_END);
	kdb.close (parent);
	kdb.open (parent);
	KeySet ks2;
	ASSERT_EQ (kdb.get (ks2, testRoot), 1);
	ASSERT_EQ (ks2.size (), 4) << "did not get key stored before" << ks;

	KeySet ks3;
	ks3.append (getAll ());
	ASSERT_EQ (kdb.set (ks3, testRoot), 1); // remove all keys
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file (not removed)";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file (not removed)";
}


TEST_F (Nested, ErrorBelow)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system:" + testRoot + "a", KEY_END));
	ks.append (Key ("system:" + testRoot + "k", KEY_END));
	ks.append (Key ("system:" + testRoot + "7", KEY_END));
	ks.append (Key ("system:" + testBelow + "a", KEY_END));
	ks.append (Key ("system:" + testBelow + "k", KEY_META, "trigger/error", "10", KEY_END));
	ks.append (Key ("system:" + testBelow + "z", KEY_END));

	ASSERT_EQ (kdb.get (ks, testRoot), 2) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 6) << "did not keep key at get" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	EXPECT_THROW (kdb.set (ks, testRoot), kdb::KDBException) << "could not trigger error";
	ASSERT_EQ (ks.size (), 6) << "did not keep key at set" << ks;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	ASSERT_EQ (stat (mpBelow->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
}
