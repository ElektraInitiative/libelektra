/**
 * @file
 *
 * @brief Tests for KDB
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <keysetio.hpp>

#include <thread> // for sleep

#include <gtest/gtest-elektra.h>


class Simple : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string configFile;

	testing::Namespaces namespaces;
	testing::MountpointPtr mp;

	Simple () : namespaces ()
	{
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

const std::string Simple::configFile = "kdbFile.dump";
const std::string Simple::testRoot = "/tests/kdb/";

TEST_F (Simple, ThrowsExceptionFail)
{
	using namespace kdb;
	Key parent(testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get(firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get(secondReturned, parent);

	firstReturned.append(Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));

	second.set(secondReturned, parent);
	first.set(firstReturned, parent); // TODO: this should throw an exception
	// EXPECT_THROW (first.set(firstReturned, parent), KDBException);
}

TEST_F (Simple, ThrowsExceptionFail2)
{
	using namespace kdb;
	Key parent(testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get(firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	second.get(secondReturned, parent);

	firstReturned.append(Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

	second.set(secondReturned, parent);
	first.set(firstReturned, parent); // TODO: this should throw an exception
	// EXPECT_THROW (first.set(firstReturned, parent), KDBException);
}

TEST_F (Simple, ThrowsExceptionFail3)
{
	using namespace kdb;
	Key parent(testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get(firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	parent = Key (testRoot, KEY_END);
	second.get(secondReturned, parent);

	firstReturned.append(Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

	parent = Key (testRoot, KEY_END);
	second.set(secondReturned, parent);
	parent = Key (testRoot, KEY_END);
	first.set(firstReturned, parent); // TODO: this should throw an exception
	// EXPECT_THROW (first.set(firstReturned, parent), KDBException);
}

TEST_F (Simple, ThrowsExceptionFail4)
{
	using namespace kdb;
	Key parent(testRoot, KEY_END);

	KDB first;
	KeySet firstReturned;
	first.get(firstReturned, parent);

	KDB second;
	KeySet secondReturned;
	parent = Key (testRoot, KEY_END);
	second.get(secondReturned, parent);

	secondReturned.append(Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	secondReturned.append(Key ("system" + testRoot + "key3", KEY_VALUE, "value3", KEY_END));

	parent = Key (testRoot, KEY_END);
	second.set(secondReturned, parent);

	parent = Key (testRoot, KEY_END);
	firstReturned.append(Key ("system" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));
	first.set(firstReturned, parent); // TODO: this should throw an exception
	// EXPECT_THROW (first.set(firstReturned, parent), KDBException);
}


TEST_F (Simple, ThrowsExceptionCorrectlyFail5)
{
	kdb::KDB kdb;
	kdb::KDB kdb2;

	kdb::KeySet ks;
	kdb.get (ks, "/tests");
	kdb2.get (ks, "/tests");
	ks.append (kdb::Key ("system/tests/key", KEY_VALUE, "value", KEY_END));
	kdb.set (ks, "/tests");
	ks.append (kdb::Key ("system/tests/key2", KEY_VALUE, "value2", KEY_END));

	std::this_thread::sleep_for (std::chrono::seconds (1));
	// EXPECT_THROW (kdb2.set (ks, "/tests"), kdb::KDBException);
}


TEST_F (Simple, GetNothing)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get (ks, testRoot);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
}

TEST_F (Simple, SetNothing)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get (ks, testRoot);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
	kdb.set (ks, testRoot);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
}

TEST_F (Simple, TryChangeAfterSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	std::string name = "system" + testRoot + "try_change";
	Key k (name, KEY_END);
	EXPECT_EQ (k.getName (), name);
	ks.append (k);
	EXPECT_THROW (k.setName ("user/x"), kdb::KeyInvalidName);
	EXPECT_EQ (k.getName (), name);
	kdb.get (ks, testRoot);
	ASSERT_EQ (ks.size (), 1) << "lost keys in get\n" << ks;
	kdb.set (ks, testRoot);
	EXPECT_THROW (k.setName ("user/x"), kdb::KeyInvalidName);
	EXPECT_EQ (k.getName (), name);
	ASSERT_EQ (ks.size (), 1) << "got no keys\n" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), 0) << "did not find config file";
}

TEST_F (Simple, MetaInSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parent (testRoot, KEY_END);
	kdb.get (ks, parent);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backend" << ks;

	ks.append (Key ("meta" + testRoot + "wrong_meta_key", KEY_META_NAME, KEY_END));

	ASSERT_EQ (ks.size (), 1) << "key not inserted:\n" << ks;
	kdb.set (ks, parent);
	printError (std::cout, parent);
	printWarnings (std::cout, parent);
	ASSERT_EQ (ks.size (), 1) << "got wrong keys:\n" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "did find config file";
}

TEST_F (Simple, InvalidKeysInSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parent (testRoot, KEY_END);
	kdb.get (ks, parent);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backend" << ks;

	ks.append (Key (testRoot + "wrong_cascading_key", KEY_END));
	ks.append (Key ("meta" + testRoot + "wrong_meta_key", KEY_META_NAME, KEY_END));

	ASSERT_EQ (ks.size (), 2) << "keys not inserted:\n" << ks;
	kdb.set (ks, parent);
	printError (std::cout, parent);
	printWarnings (std::cout, parent);
	ASSERT_EQ (ks.size (), 2) << "got wrong keys:\n" << ks;
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "did find config file";
}

kdb::KeySet getAll ()
{
	using namespace ckdb;
	return
#include <data_allns.c>
}

// TODO: replace with proper ContainerEq
void compareKeySet (kdb::KeySet ks1, kdb::KeySet ks2)
{
	using namespace kdb;
	KeySet::iterator i1 = ks1.begin ();
	for (KeySet::iterator i2 = ks2.begin (); i2 != ks2.end (); ++i1, ++i2)
	{
		EXPECT_EQ (*i1, *i2);
	}
	EXPECT_EQ (i1, ks1.end ()) << "second iterator not at end";
}

TEST_F (Simple, EverythingInGetSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks = getAll ();
	Key parent (testRoot, KEY_END);
	kdb.get (ks, parent);
	ASSERT_EQ (ks.size (), 714) << "did not keep" << ks;

	kdb.set (ks, parent);
	printError (std::cout, parent);
	printWarnings (std::cout, parent);
	ASSERT_EQ (ks.size (), 714) << "got wrong keys:\n" << ks;
	// KeySet cmp = getAll();
	// ASSERT_EQ(ks, cmp);
	// ASSERT_THAT(ks, ContainerEq(cmp));
	compareKeySet (ks, getAll ());
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "did find config file";
}

TEST_F (Simple, EverythingInSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parent (testRoot, KEY_END);
	kdb.get (ks, parent);
	ASSERT_EQ (ks.size (), 0) << "got from freshly mounted" << ks;
	ks.append (getAll ());
	ASSERT_EQ (ks.size (), 714) << "did not keep" << ks;

	kdb.set (ks, parent);
	printError (std::cout, parent);
	printWarnings (std::cout, parent);
	ASSERT_EQ (ks.size (), 714) << "got wrong keys:\n" << ks;
	// KeySet cmp = getAll();
	// ASSERT_EQ(ks, cmp);
	// ASSERT_THAT(ks, ContainerEq(cmp));
	compareKeySet (ks, getAll ());
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "did find config file";
}


TEST_F (Simple, RemoveFile)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get (ks, testRoot);
	ks.append (Key ("system" + testRoot + "remove", KEY_END));
	ASSERT_EQ (ks.size (), 1) << "could not append key\n" << ks;
	kdb.set (ks, testRoot);
	ASSERT_EQ (ks.size (), 1) << "key gone after kdb.set?\n" << ks;

	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), 0) << "found no file";

	Key parentKey;
	kdb.close (parentKey);
	kdb.open (parentKey);

	kdb.get (ks, testRoot);
	ks.clear ();
	ASSERT_EQ (ks.size (), 0) << "keyset should be empty after clearing it\n" << ks;
	kdb.set (ks, testRoot);

	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
}


TEST_F (Simple, GetNothingEmpty)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key k;
	try
	{
		ASSERT_EQ (kdb.get (ks, k), 1);
	}
	catch (std::exception const & e)
	{
		std::cout << "Could not get everything from keydatabase: " << e.what () << std::endl;
	}
	ASSERT_EQ (k.getMeta<int> ("warnings/#00/number"), 105) << "did not get warning for empty key";
	// got everything, so make no assumption of size
}

TEST_F (Simple, GetSystem)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey ("system" + testRoot, KEY_END);
	ks.append (Key (parentKey.getName () + "/key", KEY_END));
	EXPECT_NE (kdb.get (ks, parentKey), -1);
	ASSERT_EQ (ks.size (), 1) << "no key stayed" << ks;
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";

	ASSERT_NE (kdb.set (ks, parentKey), -1);
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.close (parentKey);

	KeySet ks2;
	kdb.open (parentKey);
	kdb.get (ks2, parentKey);
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
}

TEST_F (Simple, WrongStateSystem)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey ("system" + testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	kdb.close (parentKey);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
}


TEST_F (Simple, WrongStateUser)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey ("user" + testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
}


TEST_F (Simple, WrongStateCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey (testRoot, KEY_END);
	EXPECT_THROW (kdb.set (ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
}

TEST_F (Simple, GetCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey (testRoot, KEY_END);
	kdb.get (ks, parentKey);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;

	Key setParentKey ("system" + testRoot, KEY_END);
	kdb.set (ks, setParentKey);
	kdb.close (parentKey);
}


TEST_F (Simple, GetAppendCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ks.append (Key (testRoot + "key", KEY_END));
	Key parentKey (testRoot, KEY_END);
	std::string myRoot = testRoot.substr (0, testRoot.length () - 1);
	EXPECT_EQ (parentKey.getName (), myRoot);
	EXPECT_EQ (parentKey.getString (), "");
	kdb.get (ks, parentKey);
	EXPECT_EQ (parentKey.getName (), myRoot);
	std::string fn = parentKey.getString ();
	EXPECT_EQ (fn.substr (fn.find_last_of ('/') + 1), "kdbFile.dump");
	parentKey.setString ("");

	ASSERT_EQ (ks.size (), 1) << "no key stayed" << ks;
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.set (ks, parentKey);
	EXPECT_EQ (parentKey.getName (), myRoot);
	EXPECT_EQ (parentKey.getString (), "");
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.close (parentKey);
	EXPECT_EQ (parentKey.getName (), myRoot);
	EXPECT_EQ (parentKey.getString (), "");

	KeySet ks2;
	kdb.open (parentKey);
	EXPECT_EQ (parentKey.getName (), myRoot);
	EXPECT_EQ (parentKey.getString (), "");
	kdb.get (ks2, parentKey);
	EXPECT_EQ (parentKey.getName (), myRoot);
	fn = parentKey.getString ();
	EXPECT_EQ (fn.substr (fn.find_last_of ('/') + 1), "kdbFile.dump");
	ASSERT_EQ (ks2.size (), 0) << "got keys from freshly mounted backends";
}

TEST_F (Simple, GetAppendMeta)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ks.append (Key ("meta/key", KEY_META_NAME, KEY_END));
	Key parentKey (testRoot, KEY_END);
	kdb.get (ks, parentKey);
	ASSERT_EQ (ks.size (), 1) << "no key stayed";
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "meta/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.set (ks, parentKey);
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "meta/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.close (parentKey);

	KeySet ks2;
	kdb.open (parentKey);
	kdb.get (ks2, parentKey);
	ASSERT_EQ (ks2.size (), 0) << "got keys from freshly mounted backends";
}

TEST_F (Simple, GetAppendNamespaces)
{
	using namespace kdb;
	for (size_t i = 0; i < namespaces.size (); ++i)
	{
		KDB kdb;
		KeySet ks;
		ks.append (Key (namespaces[i].name + testRoot + "key", KEY_END));
		kdb.get (ks, testRoot);
		ASSERT_EQ (ks.size (), 1) << "did not got key appended first with namespace " << namespaces[i].name;
		ks.rewind ();
		ks.next ();
		EXPECT_EQ (ks.current ().getName (), namespaces[i].name + "/tests/kdb/key") << "name of element in keyset wrong";
		EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	}
}

TEST_F (Simple, SetSystemKey)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey (testRoot, KEY_END);
	ks.append (Key ("system" + testRoot + "key", KEY_END));
	kdb.get (ks, parentKey);
	ASSERT_EQ (ks.size (), 1) << "got keys from freshly mounted backends";
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system" + testRoot + "key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "") << "string of element in keyset wrong";
	kdb.set (ks, parentKey);
	kdb.close (parentKey);

	KeySet ks2;
	kdb.open (parentKey);
	kdb.get (ks2, parentKey);
	ASSERT_EQ (ks2.size (), 1) << "wrong size";
	ks2.rewind ();
	ks2.next ();
	EXPECT_EQ (ks2.current ().getName (), "system" + testRoot + "key") << "name of element in keyset wrong";
	EXPECT_EQ (ks2.current ().getString (), "") << "string of element in keyset wrong";
}

TEST_F (Simple, SetSystemGetAppend)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey (testRoot, KEY_END);
	ks.append (Key ("system" + testRoot + "key", KEY_VALUE, "value1", KEY_END));
	ASSERT_NE (kdb.get (ks, parentKey), -1);
	ASSERT_EQ (ks.size (), 1) << "got keys from freshly mounted backends";
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "value1") << "string of element in keyset wrong";
	ASSERT_EQ (kdb.set (ks, parentKey), 1);
	kdb.close (parentKey);

	KeySet ks2;
	ks2.append (Key ("system" + testRoot + "key", KEY_VALUE, "value2", KEY_END));
	kdb.open (parentKey);
	ASSERT_EQ (kdb.get (ks2, parentKey), 1);
	ASSERT_EQ (ks2.size (), 1) << "wrong size";
	ks2.rewind ();
	ks2.next ();
	EXPECT_EQ (ks2.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks2.current ().getString (), "value1") << "string of element in keyset wrong";
}

TEST_F (Simple, SetSystemGetAppend2)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey (testRoot, KEY_END);
	ks.append (Key ("system" + testRoot + "key", KEY_VALUE, "value1", KEY_END));
	kdb.get (ks, parentKey);
	ASSERT_EQ (ks.size (), 1) << "got keys from freshly mounted backends";
	ks.rewind ();
	ks.next ();
	EXPECT_EQ (ks.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks.current ().getString (), "value1") << "string of element in keyset wrong";
	kdb.set (ks, parentKey);
	kdb.close (parentKey);

	KeySet ks2;
	ks2.append (Key ("system" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));
	kdb.open (parentKey);
	kdb.get (ks2, parentKey);
	ks2.rewind ();
	ks2.next ();
	ASSERT_EQ (ks2.size (), 1) << "wrong size";
	EXPECT_EQ (ks2.current ().getName (), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ (ks2.current ().getString (), "value1") << "string of element in keyset wrong";
}

TEST_F (Simple, WrongParent)
{
	using namespace kdb;
	KDB kdb;
	Key parent ("meta", KEY_META_NAME, KEY_END);
	KeySet ks;
	EXPECT_THROW (kdb.set (ks, parent), kdb::KDBException);
	ASSERT_EQ (ks.size (), 0) << "got keys from freshly mounted backends" << ks;
}

TEST_F (Simple, TriggerError)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	EXPECT_EQ (kdb.get (ks, testRoot), 0) << "nothing to do in get";
	ks.append (Key ("system" + testRoot + "a", KEY_END));
	ks.append (Key ("system" + testRoot + "k", KEY_META, "trigger/error", "10", KEY_END));
	ks.append (Key ("system" + testRoot + "z", KEY_END));
	struct stat buf;
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "found wrong file";
	EXPECT_THROW (kdb.set (ks, testRoot), kdb::KDBException) << "could not trigger error";
	ASSERT_EQ (ks.size (), 3) << "key suddenly missing";
	EXPECT_EQ (ks.current ().getName (), "system" + testRoot + "k") << "ks should point to error key";
	ASSERT_EQ (stat (mp->systemConfigFile.c_str (), &buf), -1) << "created file even though error triggered";
}
