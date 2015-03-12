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

class Namespaces
{
public:
	struct Namespace
	{
		std::string name;
		std::string configFile;
	};

	Namespaces(std::string configFile)
	{
		Namespace n;
		n.name = "system";
		n.configFile = configFile;
		namespaces.push_back(n);
	}

	void unlinkConfigFiles()
	{
		for (size_t i=0; i<namespaces.size(); ++i)
		{
			unlink(namespaces[i].configFile.c_str());
		}
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

class Mounting
{
public:
	static void mount(std::string mountpoint, std::string configFile)
	{
		using namespace kdb;
		using namespace kdb::tools;

		Backend b;
		b.setMountpoint(Key(mountpoint, KEY_END), KeySet(0, KS_END));
		b.addPlugin("resolver");
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

	Simple() : namespaces(configFile)
	{}

	virtual void SetUp()
	{
		namespaces.unlinkConfigFiles();

		Mounting::mount(testRoot, configFile);
		Mounting::mount("spec"+testRoot, configFile);
	}

	virtual void TearDown()
	{
		Mounting::umount("spec"+testRoot);
		Mounting::umount(testRoot);

		namespaces.unlinkConfigFiles();
	}
};

const std::string Simple::configFile = "/tmp/kdbFile.dump";
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

TEST_F(Simple, GetAppendCascading)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	ks.append(Key(testRoot + "/key", KEY_END));
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
		ks.append(Key(namespaces[i].name + testRoot + "/key", KEY_END));
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
	ks.append(Key("system" + testRoot + "/key", KEY_END));
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	kdb.close(parentKey);

	KeySet ks2;
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
	ASSERT_EQ(ks2.size(), 1) << "wrong size";
	ks2.rewind();
	ks2.next();
	EXPECT_EQ(ks2.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks2.current().getString(), "") << "string of element in keyset wrong";
}

TEST_F(Simple, SetSystemGetAppend)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	Key parentKey(testRoot, KEY_END);
	ks.append(Key("system" + testRoot + "/key", KEY_VALUE, "value1", KEY_END));
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "value1") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	kdb.close(parentKey);

	KeySet ks2;
	ks2.append(Key("system" + testRoot + "/key", KEY_VALUE, "value2",  KEY_END));
	kdb.open(parentKey);
	kdb.get(ks2, parentKey);
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
	ks.append(Key("system" + testRoot + "/key", KEY_VALUE, "value1", KEY_END));
	kdb.get(ks, parentKey);
	ASSERT_EQ(ks.size(), 1) << "got keys from freshly mounted backends";
	ks.rewind();
	ks.next();
	EXPECT_EQ(ks.current().getName(), "system/tests/kdb/key") << "name of element in keyset wrong";
	EXPECT_EQ(ks.current().getString(), "value1") << "string of element in keyset wrong";
	kdb.set(ks, parentKey);
	kdb.close(parentKey);

	KeySet ks2;
	ks2.append(Key("system" + testRoot + "/key2", KEY_VALUE, "value2",  KEY_END));
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

TEST_F(Simple, WrongState)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;
	kdb.set(ks, testRoot);
	ASSERT_EQ(ks.size(), 0) << "got keys from freshly mounted backends" << ks;
}
