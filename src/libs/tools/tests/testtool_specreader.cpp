/**
 * @file
 *
 * @brief Tests for the spec readerclass
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

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
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	EXPECT_EQ (bi.getBackendConfig(),
			KeySet(5, *Key ("user/something", "here", KEY_END), KS_END));
	EXPECT_FALSE (bi.validated());
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 0);
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("a", "resolver"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("b", "storage"));
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
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "config/needs/else", "too",
					KEY_META, "infos/needs", "a b",
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
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("b", "resolver"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("a", "storage"));
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
					KEY_META, "infos/needs", "resolver storage",
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
	EXPECT_EQ (bi.begin()[0], PluginSpec("r", "resolver"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("c", "storage"));
}

TEST(SpecReader, withNeedsResolvedPreferencesPlugins)
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
					KEY_META, "infos/plugins", "b",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				*Key ("user/mp/below",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 1) << "there should be nothing added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("b", "b"));
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("b", "b"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("r", "resolver"));
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
					KEY_META, "infos/needs", "resolver storage",
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
	EXPECT_EQ (bi.begin()[0], PluginSpec("r", "resolver"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("b", "storage"));
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
					KEY_META, "infos/needs", "a", // warning: order matters here..
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("a"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("r", "resolver"));
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
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 2) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("mathcheck"));
	EXPECT_EQ (bi.begin()[1], PluginSpec("rename"));
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
	bi.resolveNeeds();
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
	bi.resolveNeeds();
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("bestcheck"));
}

TEST(SpecReader, DISABLED_pluginConfiguration)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["status"] = "popular";
	mpd->data [PluginSpec("b")] ["status"] = "popular";
	mpd->data [PluginSpec("python#transform")] ["provides"] = "transform";
	mpd->data [PluginSpec("python#transform")] ["metadata"] = "transform/python";
	mpd->data [PluginSpec("python#rename")] ["provides"] = "rename";
	mpd->data [PluginSpec("python#rename")] ["metadata"] = "rename/toupper rename/tolower";
	mpd->data [PluginSpec("python#rename")] ["status"] = "memleak";
	mpd->data [PluginSpec("ccode")] ["type"] = "virtual";
	mpd->data [PluginSpec("hexcode")] ["type"] = "virtual";
	mpd->data [PluginSpec("hexcode")] ["metadata"] = "recode";
	BackendBuilderInit mpi (mpd);
	SpecReader sr(mpi);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "config/plugin/python/something", "",
					KEY_META, "config/plugin/python/else/something", "",
					KEY_META, "config/plugin/b/something", "",
					KEY_END),
				*Key ("user/mp/below",
					KEY_META, "config/metadata/python#transform", "transform/python transform/pythonfast",
					KEY_META, "config/metadata/python#transform/script", "transform.py",
					KEY_META, "transform/python", "below = other+5",
					KEY_END),
				*Key ("user/mp/encode",
					KEY_META, "config/metadata/#0ccode", "recode othercode",
					KEY_META, "config/metadata/#1hexcode#dash", "recode othercode",
					KEY_META, "config/metadata/#1hexcode#dash/escape", "-",
					KEY_META, "recode", "first c then hex",
					KEY_END),
				*Key ("user/mp/other",
					KEY_META, "infos/needs", "python#rename", // I want to prefer python#rename, even if there is a better one
					KEY_META, "config/plugin/python#rename/otherthing", "norename", // register a new plugin with new contract
					KEY_META, "config/plugin/python#rename/script", "rename.py",
					KEY_META, "rename/toupper", "",
					// kdb mount python#transform option=fastload,script=transform.py python#rename otherthing=norename,script=rename.py
					KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.getBackends() [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint(), "user/mp");
	EXPECT_EQ (bi.getConfigFile(), "file.ini");
	ASSERT_EQ (std::distance(bi.begin(), bi.end()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin()[0], PluginSpec("bestcheck"));
}
