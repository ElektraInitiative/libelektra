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
		ks.append (Key ("system" + testRoot, KEY_END));
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

	firstReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	secondReturned.append (Key ("system" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent);
}

TEST_P (Conflict, ConflictWithFileLoop)
{
	using namespace kdb;

	Key parent (testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get (firstReturned, parent);

	KDB second;
	KeySet secondReturned;

	const int retries = 5;
	for (int j = 0; j < retries; j++)
	{
		second.get (secondReturned, parent);

		firstReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
		secondReturned.append (Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
		secondReturned.append (Key ("system" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

		for (int i = 0; i < retries; ++i)
		{
			second.set (secondReturned, parent);
			EXPECT_THROW (first.set (firstReturned, parent), KDBException);
		}
	}
	printWarnings (std::cout, parent);
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

	firstReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value2", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent);
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

	firstReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));

	second.set (secondReturned, parent);
	EXPECT_THROW (first.set (firstReturned, parent), KDBException);
	printWarnings (std::cout, parent);
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

	firstReturned.append (Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
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
	printWarnings (std::cout, parent);
}


INSTANTIATE_TEST_CASE_P (Conflict, Conflict, ::testing::Values (true, false));
