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

#include <iostream>
#include <string>
#include <kdb.hpp>
#include <gtest/gtest.h>

/**
 * @brief Easily allows to generate regression tests for keysets.
 *
 * @param tocheck the keyset to check (name + string)
 * @param name the name of the keyset
 */
void outputGTest(kdb::KeySet tocheck, std::string name)
{
	std::cout << name << ".rewind();" << std::endl;
	tocheck.rewind();
	while(tocheck.next())
	{
		std::cout << name << ".next();" << std::endl;
		std::cout << "EXPECT_TRUE(" << name
			<< ".current().getName() == \""
			<< tocheck.current().getName()
			<< "\") << \"name of element in keyset wrong\";"
			<< std::endl;
		std::cout << "EXPECT_TRUE(" << name
			<< ".current().getString() == \""
			<< tocheck.current().getString()
			<< "\") << \"string of element in keyset wrong\";"
			<< std::endl;
	}
}

TEST(MergeResult, ResolveConflictDeletesConflictMeta)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b("my_backend", "/");
	b.addPlugin("resolver");
	b.addPlugin("dump");
	b.validated();

	Key rootKey(Backends::mountpointsPath, KEY_END);
	KeySet mountConfig;
	b.serialise(rootKey, mountConfig);

	// outputGTest(mountConfig, "mountConfig");

	mountConfig.rewind();
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "serialised Backend") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/errorplugins") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/errorplugins/#5#resolver#resolver#") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/getplugins") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/getplugins/#0#resolver") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/getplugins/#5#dump#dump#") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/mountpoint") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "/") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/setplugins") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/setplugins/#0#resolver") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/setplugins/#5#dump") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
	mountConfig.next();
	EXPECT_TRUE(mountConfig.current().getName() == "system/elektra/mountpoints/my_backend/setplugins/#7#resolver") << "name of element in keyset wrong";
	EXPECT_TRUE(mountConfig.current().getString() == "") << "string of element in keyset wrong";
}
