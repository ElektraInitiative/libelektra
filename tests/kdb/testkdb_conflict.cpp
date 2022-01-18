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


class Conflict : public ::testing::TestWithParam<bool>
{
protected:
	static const std::string testRoot;
	static const std::string configFile;

	testing::Namespaces namespaces;
	testing::MountpointPtr mp;

	Conflict () : namespaces ()
	{
	}

	void createConfigFile ()
	{
		using namespace kdb;
		KDB repo;
		KeySet ks;
		repo.get (ks, testRoot);
		ks.append (Key ("system:" + testRoot, KEY_END));
		repo.set (ks, testRoot);
	}

	virtual void SetUp () override
	{
		mp.reset (new testing::Mountpoint (testRoot, configFile));
		if (GetParam ())
		{
			createConfigFile ();
		}
	}

	virtual void TearDown () override
	{
		mp.reset ();
	}
};

const std::string Conflict::configFile = "kdbFile.dump";
const std::string Conflict::testRoot = "/tests/kdb/";

TEST_P (Conflict, ConflictWithFile)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get (secondReturned, parent);

	firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system:" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	secondReturned.append (Key ("system:" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent, true, true);
}

TEST_P (Conflict, DISABLED_ConflictWithFileLoop)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;

	const int retries = 10;
	for (int j = 0; j < retries; j++)
	{
		second.get (secondReturned, parent);

		firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
		secondReturned.append (Key ("system:" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
		secondReturned.append (Key ("system:" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

		for (int i = 0; i < retries; ++i)
		{
			second.set (secondReturned, parent);
			EXPECT_THROW (first.set (firstReturned, parent), KDBException);
		}
	}
	printWarnings (std::cout, parent, true, true);
}

TEST_P (Conflict, ConflictWithFileSameKey)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get (secondReturned, parent);

	firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value2", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent, true, true);
}

TEST_P (Conflict, ConflictWithFileSameKeyValue)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get (secondReturned, parent);

	firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent, true, true);
}

TEST_P (Conflict, ConflictWithRemoval)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get (secondReturned, parent);

	firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.clear (); // remove file

	second.set (secondReturned, parent);
	if (GetParam ())
	{
		EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	}
	else
	{
		EXPECT_NO_THROW (first.set (firstReturned, parent)) << "file should be still removed, should be ok to write to a new file";
	}
	printWarnings (std::cout, parent, true, true);
}

TEST_P (Conflict, DISABLED_ConflictWithRemovalLoop)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get (secondReturned, parent);

	firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.clear (); // remove file

	second.set (secondReturned, parent);
	if (GetParam ())
	{
		EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	}
	else
	{
		EXPECT_NO_THROW (first.set (firstReturned, parent)) << "file should be still removed, should be ok to write to a new file";
	}
	const int retries = 100;
	for (int j = 0; j < retries; j++)
	{
		secondReturned.clear ();
		second.get (secondReturned, parent);
		second.set (secondReturned, parent);

		firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
		EXPECT_THROW (first.set (firstReturned, parent), KDBException) << "should be conflict with removed file";

		// create file
		first.get (firstReturned, parent);
		firstReturned.append (Key ("system:" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
		first.set (firstReturned, parent);

		secondReturned.clear ();
		EXPECT_THROW (second.set (secondReturned, parent), KDBException) << "should be conflict with existing file";
	}
	printWarnings (std::cout, parent, true, true);
}


INSTANTIATE_TEST_SUITE_P (Conflict, Conflict, ::testing::Values (true, false));
