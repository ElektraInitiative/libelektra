/**
 * @file
 *
 * @brief Tests for the spec readerclass
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

#include <specreader.hpp>

#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>

#include <gtest/gtest.h>
#include <kdb.hpp>

TEST (SpecReader, withDatabase)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["ordering"] = "d";
	mpd->data[PluginSpec ("b")]["ordering"] = "d";
	mpd->data[PluginSpec ("c")]["ordering"];
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (KeySet (5, *Key ("spec:/", KEY_END), *Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				      *Key ("spec:/mp/below", KEY_META, "config/needs/something", "here", KEY_END), KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	EXPECT_EQ (bi.getBackendConfig (), KeySet (5, *Key ("user:/something", KEY_VALUE, "here", KEY_END), KS_END));
	EXPECT_FALSE (bi.validated ());
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 0) << "there should be no plugin added";
}

TEST (SpecReader, withDatabaseRecursive)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["ordering"] = "d";
	mpd->data[PluginSpec ("b")]["ordering"] = "d";
	mpd->data[PluginSpec ("c")]["ordering"];
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (KeySet (5, *Key ("spec:/", KEY_END), *Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				      *Key ("spec:/mp/below", KEY_META, "config/needs/something", "else", KEY_END),
				      *Key ("spec:/mp/below/recursive", KEY_META, "mountpoint", "otherfile.ini", KEY_END), KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
}

TEST (SpecReader, withNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "resolver";
	mpd->data[PluginSpec ("b")]["provides"] = "storage";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (KeySet (
		5, *Key ("spec:/", KEY_END), *Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
		*Key ("spec:/mp/below", KEY_META, "config/needs/something", "here", KEY_META, "infos/needs", "resolver storage", KEY_END),
		KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	EXPECT_EQ (bi.getBackendConfig (), KeySet (5, *Key ("user:/something", KEY_VALUE, "here", KEY_END), KS_END));
	EXPECT_FALSE (bi.validated ());
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 0);
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("a", "resolver"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("b", "storage"));
}

TEST (SpecReader, withNeedsResolved)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "storage";
	mpd->data[PluginSpec ("b")]["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "config/needs/something", "here",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				*Key ("spec:/mp/below",
					KEY_META, "config/needs/else", "too",
					KEY_META, "infos/needs", "a b",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	EXPECT_EQ (bi.getBackendConfig (), KeySet (5, *Key ("user:/something", KEY_VALUE, "here", KEY_END),
						   *Key ("user:/else", KEY_VALUE, "too", KEY_END), KS_END));
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("b", "resolver"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("a", "storage"));
}

TEST (SpecReader, withNeedsResolvedPreferences)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "storage";
	mpd->data[PluginSpec ("a")]["status"] = "productive";

	mpd->data[PluginSpec ("b")]["provides"] = "storage";
	mpd->data[PluginSpec ("b")]["status"] = "productive memleak";

	mpd->data[PluginSpec ("c")]["provides"] = "storage";
	mpd->data[PluginSpec ("c")]["status"] = "productive tested/unit";

	mpd->data[PluginSpec ("r")]["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				*Key ("spec:/mp/below",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("r", "resolver"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("c", "storage"));
}

TEST (SpecReader, withNeedsResolvedPreferencesPlugins)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "storage";
	mpd->data[PluginSpec ("a")]["status"] = "productive";

	mpd->data[PluginSpec ("b")]["provides"] = "storage";
	mpd->data[PluginSpec ("b")]["status"] = "productive memleak";

	mpd->data[PluginSpec ("c")]["provides"] = "storage";
	mpd->data[PluginSpec ("c")]["status"] = "productive tested/unit";

	mpd->data[PluginSpec ("r")]["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "infos/plugins", "b",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				*Key ("spec:/mp/below",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 1) << "there should be nothing added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("b", "b"));
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("b", "b"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("r", "resolver"));
}

TEST (SpecReader, withNeedsResolvedNumerical)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "storage";
	mpd->data[PluginSpec ("a")]["status"] = "productive";

	mpd->data[PluginSpec ("b")]["provides"] = "storage";
	mpd->data[PluginSpec ("b")]["status"] = "productive memleak 501";

	mpd->data[PluginSpec ("c")]["provides"] = "storage";
	mpd->data[PluginSpec ("c")]["status"] = "productive tested/unit";

	mpd->data[PluginSpec ("r")]["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (
		KeySet (5, *Key ("spec:/", KEY_END),
			*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_META, "infos/needs", "resolver storage", KEY_END),
			*Key ("spec:/mp/below", KEY_END), KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("r", "resolver"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("c", "storage"));
}


TEST (SpecReader, withNeedsResolvedPreferencesIgnored)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["provides"] = "storage";
	mpd->data[PluginSpec ("a")]["status"] = "productive";

	mpd->data[PluginSpec ("b")]["provides"] = "storage";
	mpd->data[PluginSpec ("b")]["status"] = "productive memleak";

	mpd->data[PluginSpec ("c")]["provides"] = "storage";
	mpd->data[PluginSpec ("c")]["status"] = "productive tested/unit";

	mpd->data[PluginSpec ("r")]["provides"] = "resolver";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "infos/needs", "a", // warning: order matters here..
					KEY_END),
				*Key ("spec:/mp/below",
					KEY_META, "infos/needs", "resolver storage",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be a resolver and storage added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("a"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("r", "resolver"));
}

TEST (SpecReader, withMetadata)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("rename")]["metadata"] = "rename/toupper rename/tolower";
	mpd->data[PluginSpec ("mathcheck")]["metadata"] = "check/math";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_META, "rename/toupper", "2",
					KEY_END),
				*Key ("spec:/mp/below",
					KEY_META, "default", "5",
					KEY_META, "check/math", "below >= 3",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be plugins added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("mathcheck"));
	EXPECT_EQ (bi.begin ()[1], PluginSpec ("rename"));
}


TEST (SpecReader, withMetadataPreference)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("mathcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("mathcheck")]["status"] = "concept experimental";
	mpd->data[PluginSpec ("fastcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("fastcheck")]["status"] = "recommended experimental";
	mpd->data[PluginSpec ("bestcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("bestcheck")]["status"] = "recommended";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (KeySet (5, *Key ("spec:/", KEY_END), *Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				      *Key ("spec:/mp/below", KEY_META, "check/math", "below >= 3", KEY_END), KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("bestcheck"));
}


TEST (SpecReader, withMetadataPreferenceNumerical)
{
	using namespace kdb;
	using namespace kdb::tools;
	EXPECT_EQ (PluginDatabase::calculateStatus ("WRONG -5"), -5);
	EXPECT_EQ (PluginDatabase::calculateStatus ("-5 WRONG"), -5);
	EXPECT_EQ (PluginDatabase::calculateStatus ("5 WRONG"), 5);
	EXPECT_EQ (PluginDatabase::calculateStatus ("WRONG 5"), 5);

	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("mathcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("mathcheck")]["status"] = "popular -5";
	mpd->data[PluginSpec ("fastcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("fastcheck")]["status"] = "popular";
	mpd->data[PluginSpec ("bestcheck")]["metadata"] = "check/math";
	mpd->data[PluginSpec ("bestcheck")]["status"] = "popular 5";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	sr.readSpecification (KeySet (5, *Key ("spec:/", KEY_END), *Key ("spec:/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				      *Key ("spec:/mp/below", KEY_META, "check/math", "below >= 3", KEY_END), KS_END));
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 1) << "there should be plugins added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("bestcheck"));
}

TEST (SpecReader, pluginConfiguration)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("python#transform")]["provides"] = "transform";
	mpd->data[PluginSpec ("python#transform")]["metadata"] = "transform/python";
	mpd->data[PluginSpec ("python#transform")]["status"] = "memleak";
	// twice python does not work because of limitation in MockPluginDatabase (does not use ref)
	mpd->data[PluginSpec ("lua#rename")]["provides"] = "rename";
	mpd->data[PluginSpec ("lua#rename")]["metadata"] = "rename/toupper rename/tolower";
	mpd->data[PluginSpec ("lua#rename")]["status"] = "memleak";
	BackendBuilderInit mpi (mpd);
	SpecReader sr (mpi);
	// clang-format off
	sr.readSpecification(KeySet(5,
				*Key ("spec:/", KEY_END),
				*Key ("spec:/mp", KEY_META, "mountpoint", "file.ini",
					KEY_END),
				*Key ("spec:/mp/transform",
					KEY_META, "infos/plugins", "python#transform script=transform.py",
					KEY_META, "transform/python", "below = other+5",
					KEY_END),
				*Key ("spec:/mp/rename",
					KEY_META, "infos/plugins", "lua#rename norename=,script=rename.lua",
					KEY_META, "rename/toupper", "1",
					KEY_END),
				KS_END));
	// clang-format on
	SpecBackendBuilder bi = sr.getBackends ()[Key ("spec:/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 3);
	EXPECT_EQ (bi.getMountpoint (), "/mp");
	EXPECT_EQ (bi.getConfigFile (), "file.ini");
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be plugins added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("lua#rename", KeySet (2, *Key ("user:/script", KEY_VALUE, "rename.lua", KEY_END),
								     *Key ("user:/norename", KEY_END), KS_END)));
	EXPECT_EQ (bi.begin ()[1],
		   PluginSpec ("python#transform", KeySet (1, *Key ("user:/script", KEY_VALUE, "transform.py", KEY_END), KS_END)));
	bi.resolveNeeds ();
	ASSERT_EQ (std::distance (bi.begin (), bi.end ()), 2) << "there should be plugins added";
	EXPECT_EQ (bi.begin ()[0], PluginSpec ("lua#rename", KeySet (2, *Key ("user:/script", KEY_VALUE, "rename.lua", KEY_END),
								     *Key ("user:/norename", KEY_END), KS_END)));
	EXPECT_EQ (bi.begin ()[1],
		   PluginSpec ("python#transform", KeySet (1, *Key ("user:/script", KEY_VALUE, "transform.py", KEY_END), KS_END)));
}
