/**
 * @file
 *
 * @brief Tests for the spec readerclass
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <specreader.hpp>

#include <string>
#include <iostream>
#include <algorithm>
#include <unordered_map>

#include <kdb.hpp>
#include <gtest/gtest.h>

TEST(SpecReader, withDatabase)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["ordering"] = "d";
	mpd->data [PluginSpec("b")] ["ordering"] = "d";
	mpd->data [PluginSpec("c")] ["ordering"];
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				*Key ("user/mp/below", KEY_META, "config/needs/something", "here", KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	EXPECT_EQ (bi.getBackendConfig(),
			KeySet(5, *Key ("user/something", "here", KEY_END), KS_END));
	EXPECT_FALSE (bi.validated());
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 0) << "there should be no plugin added";
}

TEST(SpecReader, withDatabaseRecursive)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["ordering"] = "d";
	mpd->data [PluginSpec("b")] ["ordering"] = "d";
	mpd->data [PluginSpec("c")] ["ordering"];
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				*Key ("user/mp/below", KEY_META, "config/needs/something", "else", KEY_END),
				*Key ("user/mp/below/recursive", KEY_META, "mountpoint", "otherfile.ini", KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
}

TEST(SpecReader, withNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["provides"] = "resolver";
	mpd->data [PluginSpec("b")] ["provides"] = "storage";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				*Key ("user/mp/below",
					KEY_META, "config/needs/something", "here",
					KEY_META, "info/needs", "resolver storage",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	EXPECT_EQ (bi.getBackendConfig(),
			KeySet(5, *Key ("user/something", "here", KEY_END), KS_END));
	EXPECT_FALSE (bi.validated());
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("storage"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("resolver"));
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("b"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("a"));
}

TEST(SpecReader, withNeedsResolved)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["provides"] = "storage";
	mpd->data [PluginSpec("b")] ["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "config/needs/something", "here",
					KEY_META, "info/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "config/needs/else", "too",
					KEY_META, "info/needs", "a b",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	EXPECT_EQ (bi.getBackendConfig(), KeySet(5,
			*Key ("user/something", "here", KEY_END),
			*Key ("user/else", "too", KEY_END),
			KS_END));
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("a"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("b"));
}

