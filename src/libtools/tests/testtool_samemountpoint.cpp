/**
 * \file
 *
 * \brief Tests for the Backend class
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <keysetio.hpp>

#include <gtest/gtest.h>

TEST(SameMountpoint, notSame)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;
	Key rootKey("user/mp", KEY_END);
	EXPECT_EQ(rootKey.getName(), "user/mp");
	EXPECT_EQ(rootKey.getString(), "");

	Backend b1;
	b1.setMountpoint(Key("user/hello", KEY_END), ks);
	EXPECT_EQ(b1.getMountpoint(), "user/hello");
	b1.serialise(rootKey, ks);
	EXPECT_EQ(rootKey.getName(), "user/mp");
	EXPECT_EQ(rootKey.getString(), "");

	Backend b2;
	b2.setMountpoint(Key("user/else", KEY_END), ks);
	EXPECT_EQ(b2.getMountpoint(), "user/else");
	b2.serialise(rootKey, ks);
	EXPECT_EQ(rootKey.getName(), "user/mp");
	EXPECT_EQ(rootKey.getString(), "");

	Backend b3;
	b2.setMountpoint(Key("/somewhere", KEY_END), ks);
	EXPECT_EQ(b2.getMountpoint(), "/somewhere");
	b2.serialise(rootKey, ks);
}

TEST(SameMountpoint, exactlySame)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;
	Key rootKey("user/mp", KEY_END);
	EXPECT_EQ(rootKey.getName(), "user/mp");
	EXPECT_EQ(rootKey.getString(), "");

	Backend b1;
	b1.setMountpoint(Key("user/hello", KEY_END), ks);
	EXPECT_EQ(b1.getMountpoint(), "user/hello");
	b1.serialise(rootKey, ks);

	Backend b2;
	b2.setMountpoint(Key("user/hello", KEY_END), ks);
	EXPECT_EQ(b2.getMountpoint(), "user/hello");
	b2.serialise(rootKey, ks);

	std::cout << ks << std::endl;

	Backend b3;
	b2.setMountpoint(Key("/somewhere", KEY_END), ks);
	EXPECT_EQ(b2.getMountpoint(), "/somewhere");
	b2.serialise(rootKey, ks);
}

