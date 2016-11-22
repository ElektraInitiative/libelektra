/**
 * @file
 *
 * @brief Tests for KDB
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <keysetio.hpp>

#include <gtest/gtest-elektra.h>

#include <stdio.h>

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

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error/number"));
	EXPECT_EQ (parentKey.getMeta<int> ("error/number"), 10);

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

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error/number"));
	EXPECT_EQ (parentKey.getMeta<int> ("error/number"), 10);

	ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", "110", KEY_END));

	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error (again)";

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error/number"));
	EXPECT_EQ (parentKey.getMeta<int> ("error/number"), 110);

	ASSERT_EQ (ks.size (), 1) << "did not keep key at set (again)" << ks;
}

TEST_F (Error, AgainRepeat)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", "10", KEY_END));

	ASSERT_EQ (kdb.get (ks, testRoot), 0) << "should be nothing to update";

	Key parentKey (testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error";

	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error"));
	EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error/number"));
	EXPECT_EQ (parentKey.getMeta<int> ("error/number"), 10);

	for (int i = 0; i < 10; ++i)
	{
		ks.append (Key ("system" + testRoot + "key", KEY_END));

		EXPECT_NO_THROW (kdb.set (ks, parentKey)) << "no error trigger?";

		ks.append (Key ("system" + testRoot + "key", KEY_META, "trigger/error", std::to_string (119 + i).c_str (), KEY_END));

		EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "could not trigger error (again)";

		EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error"));
		EXPECT_TRUE (parentKey.getMeta<const kdb::Key> ("error/number"));
		EXPECT_EQ (parentKey.getMeta<int> ("error/number"), 119 + i) << " with reason: "
									     << parentKey.getMeta<std::string> ("error/reason");

		ASSERT_EQ (ks.size (), 1) << "did not keep key at set (again)" << ks;
	}
}


TEST_F (Error, CSimple)
{
	using namespace ckdb;
	Key * parentKey = keyNew (testRoot.c_str (), KEY_END);
	KDB * kdb = kdbOpen (parentKey);
	KeySet * ks = ksNew (20, KS_END);

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

TEST_F (Error, ToWarning)
{
	using namespace ckdb;
	Key * parentKey = keyNew (testRoot.c_str (), KEY_END);
	KDB * kdb = kdbOpen (parentKey);
	KeySet * ks = ksNew (20, KS_END);

	ksAppendKey (ks, keyNew (("system" + testRoot + "key1").c_str (), KEY_META, "trigger/error/nofail", "10", KEY_END));
	ksAppendKey (ks, keyNew (("system" + testRoot + "key2").c_str (), KEY_META, "trigger/error", "110", KEY_END));

	ASSERT_EQ (kdbGet (kdb, ks, parentKey), 0) << "should be nothing to update";
	ASSERT_EQ (ksGetSize (ks), 2) << "did not keep key at get" << ks;

	EXPECT_EQ (kdbSet (kdb, ks, parentKey), -1) << "could not trigger error";

	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "error"));
	EXPECT_STREQ (keyString (ckdb::keyGetMeta (parentKey, "error/number")), "10");

	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "warnings/#00"));
	EXPECT_STREQ (keyString (ckdb::keyGetMeta (parentKey, "warnings/#00/number")), "110");


	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (ks);
}

TEST_F (Error, Persists)
{
	using namespace ckdb;
	Key * parentKey = keyNew (testRoot.c_str (), KEY_END);
	KDB * kdb = kdbOpen (parentKey);
	KeySet * ks = ksNew (20, KS_END);

	ksAppendKey (ks, keyNew (("system" + testRoot + "key").c_str (), KEY_META, "trigger/error", "10", KEY_END));

	ASSERT_EQ (kdbGet (kdb, ks, parentKey), 0) << "should be nothing to update";
	ASSERT_EQ (ksGetSize (ks), 1) << "did not keep key at get" << ks;

	EXPECT_EQ (kdbSet (kdb, ks, parentKey), -1) << "could not trigger error";

	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "error"));
	EXPECT_STREQ (keyString (ckdb::keyGetMeta (parentKey, "error/number")), "10");


	keyDel (ksLookup (ks, keyNew (("system" + testRoot + "key").c_str (), KEY_END), KDB_O_POP | KDB_O_DEL));

	EXPECT_EQ (kdbSet (kdb, ks, parentKey), 0) << "kdbSet failed";
	EXPECT_TRUE (ckdb::keyGetMeta (parentKey, "error"));
	EXPECT_STREQ (keyString (ckdb::keyGetMeta (parentKey, "error/number")), "10");


	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (ks);
}
