/**
 * @file
 *
 * @brief Tests for KDB
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <keysetio.hpp>

#include <gtest/gtest-elektra.h>


class Error : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string configFileRoot;


	testing::Namespaces namespaces;
	testing::MountpointPtr mpRoot;

	Error () : namespaces ()
	{
	}

	virtual void SetUp () override
	{
		mpRoot.reset (new testing::Mountpoint (testRoot, configFileRoot));
	}

	virtual void TearDown () override
	{
		mpRoot.reset ();
	}
};

const std::string Error::testRoot = "/tests/kdb/";
const std::string Error::configFileRoot = "kdbFileError.dump";


TEST_F (Error, Simple)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", "10", KEY_END));

	ASSERT_EQ (kdb.get (ks, testRoot), 0) << "should be nothing to update";
	ASSERT_EQ (ks.size (), 1) << "did not keep key at get" << ks;

	Key parentKey (testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error";
	struct stat buf;
	ASSERT_EQ (stat (mpRoot->systemConfigFile.c_str (), &buf), -1) << "found wrong file";

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error/number"));
	EXPECT_EQ (parentKey.getMeta<int>("error/number"), 10);

	ASSERT_EQ (ks.size (), 1) << "did not keep key at set" << ks;
}

TEST_F (Error, Again)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", "10", KEY_END));

	ASSERT_EQ (kdb.get (ks, testRoot), 0) << "should be nothing to update";

	Key parentKey (testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error";

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error/number"));
	EXPECT_EQ (parentKey.getMeta<int>("error/number"), 10);

	ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", "110", KEY_END));

	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error (again)";

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key>("error/number"));
	EXPECT_EQ (parentKey.getMeta<int>("error/number"), 110);

	ASSERT_EQ (ks.size (), 1) << "did not keep key at set (again)" << ks;
}


TEST_F (Error, CSimple)
{
	using namespace ckdb;
	Key *parentKey = keyNew (testRoot.c_str (), KEY_END);
	KDB *kdb = kdbOpen (parentKey);
	KeySet *ks = ksNew (20, KS_END);

	ksAppendKey (ks, keyNew (("system" + testRoot + "key").c_str (), KEY_META, "trigger/error", "10", KEY_END));

	ASSERT_EQ (kdbGet (kdb, ks, parentKey), 0) << "should be nothing to update";
	ASSERT_EQ (ksGetSize (ks), 1) << "did not keep key at get" << ks;

	EXPECT_EQ (kdbSet (kdb, ks, parentKey), -1) << "could not trigger error";

	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "error"));
	EXPECT_STREQ (keyString (ckdb::keyGetMeta (parentKey, "error/number")), "10");

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (ks);
}
