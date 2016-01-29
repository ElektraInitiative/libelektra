/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

#include <backendbuilder.hpp>

#include <backend.hpp>
#include <backends.hpp>
#include <plugindatabase.hpp>

#include <string>
#include <iostream>
#include <algorithm>
#include <unordered_map>

#include <kdb.hpp>
#include <gtest/gtest.h>

TEST(BackendBuilder, withDatabase)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["ordering"] = "d";
	mpd->data[PluginSpec("b")]["ordering"] = "d";
	mpd->data[PluginSpec("c")]["ordering"];
	BackendBuilderInit bbi(mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("a"));
	bb.addPlugin(PluginSpec("b"));
	bb.addPlugin(PluginSpec("c"));
}


TEST(MountBackendBuilder, basicAddRem)
{
	using namespace kdb;
	using namespace kdb::tools;
	try {
		Backend b;
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("dump"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what() << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin(PluginSpec("resolver"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(PluginSpec("dump"));
	EXPECT_TRUE(bb.validated());

	bb.remPlugin(PluginSpec("dump"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(PluginSpec("dump"));
	EXPECT_TRUE(bb.validated());
}

TEST(MountBackendBuilder, basicSort)
{
	using namespace kdb;
	using namespace kdb::tools;
	try {
		Backend b;
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("glob"));
		b.addPlugin(PluginSpec("keytometa"));
		b.addPlugin(PluginSpec("augeas"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what() << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin(PluginSpec("resolver"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(PluginSpec("keytometa"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(PluginSpec("glob"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(PluginSpec("augeas"));
	EXPECT_TRUE(bb.validated()) << "Reordering not successful?";
}


TEST(MountBackendBuilder, allSort)
{
	using namespace kdb;
	using namespace kdb::tools;
	try {
		Backend b;
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("glob"));
		b.addPlugin(PluginSpec("keytometa"));
		b.addPlugin(PluginSpec("augeas"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what() << std::endl;
		return;
	}

	std::vector <std::string> permutation = {"augeas", "glob", "keytometa", "resolver"};

	do {
		// for (auto const & p : permutation) std::cout << p << " ";
		// std::cout << std::endl;
		MountBackendBuilder bb;
		bb.addPlugin(PluginSpec(permutation[0]));
		bb.addPlugin(PluginSpec(permutation[1]));
		bb.addPlugin(PluginSpec(permutation[2]));
		bb.addPlugin(PluginSpec(permutation[3]));
		EXPECT_TRUE(bb.validated()) << "Reordering not successful?";
	} while (std::next_permutation(permutation.begin(), permutation.end()));
}


TEST(MountBackendBuilder, resolveNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	try {
		Backend b;
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("line"));
		b.addPlugin(PluginSpec("null"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what() << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin(PluginSpec("resolver"));
	EXPECT_FALSE(bb.validated()) << "resolver+null should be missing";
	bb.addPlugin(PluginSpec("line"));
	EXPECT_FALSE(bb.validated()) << "null should be missing";
	bb.resolveNeeds();
	EXPECT_TRUE(bb.validated()) << "Did not add null automatically";
}


TEST(BackendBuilder, resolveDoubleNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["needs"] = "c v";
	mpd->data[PluginSpec("c")]["provides"] = "v";
	mpd->data[PluginSpec("resolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("resolver"));
	bb.addPlugin(PluginSpec("a"));
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 2);
	bb.resolveNeeds();
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 3);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("resolver"));
	EXPECT_EQ(bb.cbegin()[1], PluginSpec("a"));
	EXPECT_EQ(bb.cbegin()[2], PluginSpec("c"));
}

TEST(BackendBuilder, resolveDoubleNeedsVirtual)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["needs"] = "v c";
	mpd->data[PluginSpec("c")]["provides"] = "v";
	mpd->data[PluginSpec("resolver")]["provides"] = "resolver";
	EXPECT_EQ (mpd->lookupInfo(PluginSpec("c"), "provides"), "v");
	EXPECT_EQ (mpd->lookupInfo(PluginSpec("c", "v"), "provides"), "v");
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("resolver"));
	bb.addPlugin(PluginSpec("a"));
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 2);
	bb.resolveNeeds();
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 3);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("resolver"));
	EXPECT_EQ(bb.cbegin()[1], PluginSpec("a"));
	EXPECT_EQ(bb.cbegin()[2], PluginSpec("c", "v")) << "remember it was virtual";
}

TEST(BackendBuilder, doubleAddWithConf)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["needs"] = "v c";
	mpd->data[PluginSpec("c")]["provides"] = "v";
	mpd->data[PluginSpec("resolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("resolver"));
	bb.addPlugin(PluginSpec("a"));
	bb.addPlugin(PluginSpec("c", KeySet(2, *Key("user/abc", KEY_END), KS_END)));
	bb.addPlugin(PluginSpec("v", KeySet(2, *Key("user/vef", KEY_END), KS_END)));
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 4);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("resolver"));
	EXPECT_EQ(bb.cbegin()[1], PluginSpec("a"));
	EXPECT_EQ(bb.cbegin()[2], PluginSpec("c",
		KeySet(2, *Key("user/abc", KEY_END),
			  KS_END)));
	EXPECT_EQ(bb.cbegin()[3], PluginSpec("c", "v",
		KeySet(2, *Key("user/vef", KEY_END),
			  KS_END))) << "remember it was virtual";
	bb.resolveNeeds();
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 4);
}


TEST(BackendBuilder, doubleAddWithConfVirtual)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["needs"] = "v c";
	mpd->data[PluginSpec("c")]["provides"] = "v";
	mpd->data[PluginSpec("noresolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("resolver"));
	bb.addPlugin(PluginSpec("a"));
	bb.addPlugin(PluginSpec("v", KeySet(2, *Key("user/vef", KEY_END), KS_END)));
	bb.addPlugin(PluginSpec("c", KeySet(2, *Key("user/abc", KEY_END), KS_END)));
	ASSERT_EQ(std::distance(bb.cbegin(), bb.cend()), 4);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("noresolver", "resolver"));
	EXPECT_EQ(bb.cbegin()[1], PluginSpec("a"));
	EXPECT_EQ(bb.cbegin()[2], PluginSpec("c", "v",
		KeySet(2, *Key("user/vef", KEY_END),
			  KS_END)));
	EXPECT_EQ(bb.cbegin()[3], PluginSpec("c",
		KeySet(2, *Key("user/abc", KEY_END),
			  KS_END)));
	bb.resolveNeeds();
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 4);
}

TEST(BackendBuilder, directPluginLoading)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("a")]["plugins"] = "x a=b";
	mpd->data[PluginSpec("a")]["needs"] = "resolver";
	mpd->data[PluginSpec("x")]["provides"] = "x";
	mpd->data[PluginSpec("noresolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin(PluginSpec("a"));
	ASSERT_EQ(std::distance(bb.cbegin(), bb.cend()), 1);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("a"));
	bb.resolveNeeds();
	EXPECT_EQ(std::distance(bb.cbegin(), bb.cend()), 3);
	EXPECT_EQ(bb.cbegin()[0], PluginSpec("a"));
	EXPECT_EQ(bb.cbegin()[1], PluginSpec("x",
		KeySet(2, *Key("user/a", KEY_VALUE, "b", KEY_END),
			  KS_END)));
	EXPECT_EQ(bb.cbegin()[2], PluginSpec("noresolver", "resolver"));
}
