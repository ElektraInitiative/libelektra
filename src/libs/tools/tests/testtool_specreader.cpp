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
	EXPECT_EQ (bi.begin()[0], PluginSpec("resolver"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("storage"));
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("a"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("b"));
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
	EXPECT_EQ (bi.begin()[0], PluginSpec("b"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("a"));
}

TEST(SpecReader, withNeedsResolvedPreferences)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["provides"] = "storage";
	mpd->data [PluginSpec("a")] ["status"] = "productive";

	mpd->data [PluginSpec("b")] ["provides"] = "storage";
	mpd->data [PluginSpec("b")] ["status"] = "productive memleak";

	mpd->data [PluginSpec("c")] ["provides"] = "storage";
	mpd->data [PluginSpec("c")] ["status"] = "productive tested";

	mpd->data [PluginSpec("r")] ["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "info/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("r"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("c"));
}

TEST(SpecReader, withNeedsResolvedNumerical)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["provides"] = "storage";
	mpd->data [PluginSpec("a")] ["status"] = "productive";

	mpd->data [PluginSpec("b")] ["provides"] = "storage";
	mpd->data [PluginSpec("b")] ["status"] = "productive memleak 501";

	mpd->data [PluginSpec("c")] ["provides"] = "storage";
	mpd->data [PluginSpec("c")] ["status"] = "productive tested";

	mpd->data [PluginSpec("r")] ["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "info/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("r"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("b"));
}



TEST(SpecReader, withNeedsResolvedPreferencesIgnored)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["provides"] = "storage";
	mpd->data [PluginSpec("a")] ["status"] = "productive";

	mpd->data [PluginSpec("b")] ["provides"] = "storage";
	mpd->data [PluginSpec("b")] ["status"] = "productive memleak";

	mpd->data [PluginSpec("c")] ["provides"] = "storage";
	mpd->data [PluginSpec("c")] ["status"] = "productive tested";

	mpd->data [PluginSpec("r")] ["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "info/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "info/needs", "a",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("r"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("a"));
}

TEST(SpecReader, withMetadata)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("rename")] ["metadata"] = "rename/toupper rename/tolower";
	mpd->data [PluginSpec("mathcheck")] ["metadata"] = "check/math";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "rename/toupper", "2",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "default", "5",
					KEY_META, "check/math", "below >= 3",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("rename"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("mathcheck"));
}


TEST(SpecReader, withMetadataPreference)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("mathcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("mathcheck")] ["status"] = "concept unfinished";
	mpd->data [PluginSpec("fastcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("fastcheck")] ["status"] = "popular preview";
	mpd->data [PluginSpec("bestcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("bestcheck")] ["status"] = "popular";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "check/math", "below >= 3",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("bestcheck"));
}


TEST(SpecReader, withMetadataPreferenceNumerical)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("mathcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("mathcheck")] ["status"] = "popular -5";
	mpd->data [PluginSpec("fastcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("fastcheck")] ["status"] = "popular";
	mpd->data [PluginSpec("bestcheck")] ["metadata"] = "check/math";
	mpd->data [PluginSpec("bestcheck")] ["status"] = "popular 5";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "check/math", "below >= 3",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("bestcheck"));
}
