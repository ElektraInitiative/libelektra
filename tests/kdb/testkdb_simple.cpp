/**
 * \file
 *
 * \brief Tests for the Backend class
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <backend.hpp>
#include <backends.hpp>

#include <keysetio.hpp>

#include <gtest/gtest.h>

#include <kdbconfig.h>

class Namespaces
{
public:
	struct Namespace
	{
		std::string name;
	};

	Namespaces()
	{
		Namespace n;
		n.name = "system";
		namespaces.push_back(n);

		n.name = "spec";
		namespaces.push_back(n);

		n.name = "user";
		namespaces.push_back(n);
	}

	Namespace & operator[](size_t i)
	{
		return namespaces[i];
	}

	size_t size()
	{
		return namespaces.size();
	}

	std::vector<Namespace> namespaces;
};

/**
 * @brief Cascading + Spec mountpoint
 *
 * Hardcoded with resolver+dump
 *
 * useful to quickly mount something in tests
 * and determine the config file pathes (+unlink provided)
 */
class Mountpoint
{
public:
	std::string mountpoint;
	std::string userConfigFile;
	std::string specConfigFile;
	std::string systemConfigFile;
	std::string dirConfigFile; // currently unused, but may disturb tests if present

	Mountpoint(std::string mountpoint_, std::string configFile_) :
		mountpoint(mountpoint_)
	{
		unlink();
		mount(mountpoint, configFile_);
		mount("spec/" + mountpoint, configFile_);

		userConfigFile = getConfigFileName("user", mountpoint);
		specConfigFile = getConfigFileName("spec", mountpoint);
		systemConfigFile = getConfigFileName("system", mountpoint);
		dirConfigFile = getConfigFileName("dir", mountpoint);
		// std::cout << "config files are: " << dirConfigFile << " "
		// 	<< userConfigFile << " " << specConfigFile << " "
		// 	<< systemConfigFile << std::endl;
	}

	~Mountpoint()
	{
		umount("spec/" + mountpoint);
		umount(mountpoint);
		unlink();
	}

	void unlink()
	{
		::unlink(userConfigFile.c_str());
		::unlink(systemConfigFile.c_str());
		::unlink(specConfigFile.c_str());
	}

	static std::string getConfigFileName(std::string ns, std::string mp)
	{
		using namespace kdb;
		using namespace kdb;
		using namespace kdb::tools;

		using namespace kdb::tools;

		KDB kdb;
		Key parent(ns+"/"+mp, KEY_END);
		KeySet ks;
		kdb.get(ks, parent);
		return parent.getString();
	}

	static void mount(std::string mountpoint, std::string configFile)
	{
		using namespace kdb;
		using namespace kdb::tools;

		Backend b;
		b.setMountpoint(Key(mountpoint, KEY_END), KeySet(0, KS_END));
		b.addPlugin(KDB_DEFAULT_RESOLVER);
		b.useConfigFile(configFile);
		b.addPlugin("dump");
		KeySet ks;
		KDB kdb;
		Key parentKey("system/elektra/mountpoints", KEY_END);
		kdb.get(ks, parentKey);
		b.serialize(ks);
		kdb.set(ks, parentKey);
	}

	static void umount(std::string mountpoint)
	{
		using namespace kdb;
		using namespace kdb::tools;
		KeySet ks;
		KDB kdb;
		Key parentKey("system/elektra/mountpoints", KEY_END);
		kdb.get(ks, parentKey);
		Backends::umount(mountpoint, ks);
		kdb.set(ks, parentKey);
	}
};

class Simple : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string configFile;

	Namespaces namespaces;
#if __cplusplus > 199711L
	std::unique_ptr<Mountpoint> mp;
#else
	std::auto_ptr<Mountpoint> mp;
#endif

	Simple() : namespaces()
	{}

	virtual void SetUp()
	{
		mp.reset(new Mountpoint(testRoot, configFile));
	}

	virtual void TearDown()
	{
		mp.reset();
	}
};

const std::string Simple::configFile = "kdbFile.dump";
const std::string Simple::testRoot = "/tests/kdb/";

std::string makeLiteralString(std::string str)
{
	std::string ret;
	for (size_t i=0; i<str.length(); ++i)
	{
		if (str[i] == '\\')
		{
			ret += "\\\\";
		} else {
			ret += str[i];
		}
	}
	return ret;
}


void outputGTest(kdb::KeySet tocheck, std::string name)
{
	std::cout << "ASSERT_EQ(" << name << ".size(), "
		<< tocheck.size() << ") << \"wrong size\" << ks;"
		<< std::endl;
	std::cout << name << ".rewind();" << std::endl;
	tocheck.rewind();
	while(tocheck.next())
	{
		std::cout << name << ".next();" << std::endl;
		std::cout << "EXPECT_EQ(" << name
			<< ".current().getName(), \""
			<< makeLiteralString(tocheck.current().getName())
			<< "\") << \"name of element in keyset wrong\";"
			<< std::endl;
		std::cout << "EXPECT_EQ(" << name
			<< ".current().getString(), \""
			<< makeLiteralString(tocheck.current().getString())
			<< "\") << \"string of element in keyset wrong\";"
			<< std::endl;
	}
}

TEST_F(Simple, GetNothing)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get(ks, testRoot);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}

TEST_F(Simple, SetNothing)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get(ks, testRoot);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
	kdb.set(ks, testRoot);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
	struct stat buf;
	ASSERT_EQ(stat(mp->systemConfigFile.c_str(), &buf), -1) << "found wrong file";
}

TEST_F(Simple, TryChangeAfterSet)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key k("system" + testRoot + "try_change", KEY_END);
	ks.append(k);
	EXPECT_THROW(k.setName("user/x"), kdb::KeyInvalidName);
	kdb.get(ks, testRoot);
	ASSERT_EQ(ks.size(), 1) << "got no keys" << ks;
	kdb.set(ks, testRoot);
	EXPECT_THROW(k.setName("user/x"), kdb::KeyInvalidName);
	ASSERT_EQ(ks.size(), 1) << "got no keys" << ks;
	struct stat buf;
	ASSERT_EQ(stat(mp->systemConfigFile.c_str(), &buf), 0) << "did not find config file";
}


TEST_F(Simple, RemoveFile)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.get(ks, testRoot);
	ks.append(Key("system" + testRoot + "remove", KEY_END));
	ASSERT_EQ(ks.size(), 1) << "could not append key\n" << ks;
	kdb.set(ks, testRoot);
	ASSERT_EQ(ks.size(), 1) << "key gone after kdb.set?\n" << ks;

	struct stat buf;
	ASSERT_EQ(stat(mp->systemConfigFile.c_str(), &buf), 0) << "found no file";

	Key parentKey;
	kdb.close(parentKey);
	kdb.open(parentKey);

	kdb.get(ks, testRoot);
	ks.clear();
	ASSERT_EQ(ks.size(), 0) << "keyset should be empty after clearing it\n" << ks;
	kdb.set(ks, testRoot);

	ASSERT_EQ(stat(mp->systemConfigFile.c_str(), &buf), -1) << "found wrong file";
}


TEST_F(Simple, GetNothingEmpty)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key k;
	ASSERT_EQ(kdb.get(ks, k), 1);
	ASSERT_EQ(k.getMeta<int>("warnings/#00/number"), 105) << "did not get warning for empty key";
	// got everything, so make no assumption of size
}

TEST_F(Simple, GetSystem)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey("system" + testRoot, KEY_END);
	ks.append(Key(parentKey.getName() + "/key", KEY_END));
	EXPECT_NE(kdb.get(ks, parentKey), -1);
	ASSERT_EQ(ks.size(), 1) << "no key stayed" << ks;
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";

	ASSERT_NE(kdb.set(ks, parentKey), -1);
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.close(parentKey);

	KeySet ks2;
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
}

TEST_F(Simple, WrongStateSystem)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey("system" + testRoot, KEY_END);
	EXPECT_THROW(kdb.set(ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	kdb.close(parentKey);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}


TEST_F(Simple, WrongStateUser)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey("user" + testRoot, KEY_END);
	EXPECT_THROW(kdb.set(ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}


TEST_F(Simple, WrongStateCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	EXPECT_THROW(kdb.set(ks, parentKey), kdb::KDBException) << "kdb set without prior kdb get should have 107 Wrong State";
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}

TEST_F(Simple, GetCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;

	Key setParentKey("system"+testRoot, KEY_END);
	kdb.set(ks, setParentKey);
	kdb.close(parentKey);
}


TEST_F(Simple, GetAppendCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ks.append(Key(testRoot + "key", KEY_END));
	Key parentKey(testRoot, KEY_END);
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "no key stayed" << ks;
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.close(parentKey);

	KeySet ks2;
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ASSERT_EQ(ks2.size(), 0) << "got keys from freshly mounted backends";
}

TEST_F(Simple, GetAppendMeta)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ks.append(Key("meta/key", KEY_META_NAME, KEY_END));
	Key parentKey(testRoot, KEY_END);
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "no key stayed";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "meta/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "meta/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.close(parentKey);

	KeySet ks2;
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ASSERT_EQ(ks2.size(), 0) << "got keys from freshly mounted backends";
}

TEST_F(Simple, GetAppendNamespaces)
{
	using namespace kdb;
	for (size_t i=0; i<namespaces.size(); ++i)
	{
		KDB kdb;
		KeySet ks;
		ks.append(Key(namespaces[i].name + testRoot + "key", KEY_END));
		kdb.get(ks, testRoot);
		ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends with namespace " << namespaces[i].name;
		ks.rewind();
		ks.next();
		EXPECT_EQ(ks.current().getName(), namespaces[i].name + "/tests/kdb/key") << "name of element in keyset wrong";
		EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	}
}

TEST_F(Simple, SetSystemKey)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	ks.append(Key("system" + testRoot + "key", KEY_END));
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system" + testRoot + "key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	kdb.close(parentKey);

	KeySet ks2;
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ASSERT_EQ(ks2.size(), 1) << "wrong size";
	ks2.rewind();
	ks2.next();
	EXPECT_EQ(ks2.current().getName(), "system" + testRoot + "key") << "name of element in keyset wrong";
	EXPECT_EQ(ks2.current().getString(), "") << "string of element in keyset wrong";
}

TEST_F(Simple, SetSystemGetAppend)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	ks.append(Key("system" + testRoot + "key", KEY_VALUE, "value1", KEY_END));
	ASSERT_NE(kdb.get(ks, parentKey), -1);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "value1") << "string of element in keyset wrong";
	ASSERT_EQ(kdb.set(ks, parentKey), 1);
	kdb.close(parentKey);

	KeySet ks2;
	ks2.append(Key("system" + testRoot + "key", KEY_VALUE, "value2",  KEY_END));
	kdb.open(parentKey);
	ASSERT_EQ(kdb.get(ks2, parentKey), 1);
	ASSERT_EQ(ks2.size(), 1) << "wrong size";
	ks2.rewind();
	ks2.next();
	EXPECT_EQ(ks2.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks2.current().getString(), "value1") << "string of element in keyset wrong";
}

TEST_F(Simple, SetSystemGetAppend2)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	ks.append(Key("system" + testRoot + "key", KEY_VALUE, "value1", KEY_END));
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "value1") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	kdb.close(parentKey);

	KeySet ks2;
	ks2.append(Key("system" + testRoot + "key2", KEY_VALUE, "value2",  KEY_END));
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ks2.rewind();
	ks2.next();
	ASSERT_EQ(ks2.size(), 1) << "wrong size";
	EXPECT_EQ(ks2.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks2.current().getString(), "value1") << "string of element in keyset wrong";
}

TEST_F(Simple, WrongParent)
{
	using namespace kdb;
	KDB kdb;
	Key parent("meta", KEY_META_NAME, KEY_END);
	KeySet ks;
	EXPECT_THROW(kdb.set(ks, parent), kdb::KDBException);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}
