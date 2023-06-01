/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

#include <backendbuilder.hpp>

#include <backend.hpp>
#include <backends.hpp>
#include <plugindatabase.hpp>

#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>

#include <gtest/gtest.h>
#include <internal/config.h>
#include <internal/utility/old_helper.h>
#include <kdb.hpp>

// We disable certain tests on ASAN enabled builds: https://travis-ci.org/sanssecours/elektra/jobs/418573941
#ifdef ENABLE_ASAN
#define GTEST_DISABLE_ASAN(name) DISABLED_##name
#else
#define GTEST_DISABLE_ASAN(name) name
#endif

TEST (BackendBuilder, withDatabase)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["ordering"] = "c";
	mpd->data[PluginSpec ("b")]["ordering"] = "c";
	mpd->data[PluginSpec ("c")]["ordering"];
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("a"));
	bb.addPlugin (PluginSpec ("b"));
	bb.addPlugin (PluginSpec ("c"));

	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("b"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c"));
}


TEST (BackendBuilder, withDatabaseIrrelevantDep)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["ordering"] = "d";
	mpd->data[PluginSpec ("b")]["ordering"] = "d";
	mpd->data[PluginSpec ("c")]["ordering"];
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("a"));
	bb.addPlugin (PluginSpec ("b"));
	bb.addPlugin (PluginSpec ("c"));

	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("b"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c"));
}


TEST (MountBackendBuilder, basicAddRem)
{
	using namespace kdb;
	using namespace kdb::tools;
	try
	{
		Backend b;
		b.addPlugin (PluginSpec ("resolver"));
		b.addPlugin (PluginSpec ("dump"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what () << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin (PluginSpec ("resolver"));
	EXPECT_FALSE (bb.validated ());

	bb.addPlugin (PluginSpec ("dump"));
	EXPECT_TRUE (bb.validated ());

	bb.remPlugin (PluginSpec ("dump"));
	EXPECT_FALSE (bb.validated ());

	bb.addPlugin (PluginSpec ("dump"));
	EXPECT_TRUE (bb.validated ());
}

TEST (GTEST_DISABLE_ASAN (MountBackendBuilder), basicSort)
{
	using namespace kdb;
	using namespace kdb::tools;
	try
	{
		Backend b;
		b.addPlugin (PluginSpec ("resolver"));
		b.addPlugin (PluginSpec ("glob"));
		b.addPlugin (PluginSpec ("keytometa"));
		b.addPlugin (PluginSpec ("augeas"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what () << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin (PluginSpec ("resolver"));
	EXPECT_FALSE (bb.validated ());

	bb.addPlugin (PluginSpec ("keytometa"));
	EXPECT_FALSE (bb.validated ());

	bb.addPlugin (PluginSpec ("glob"));
	EXPECT_FALSE (bb.validated ());

	bb.addPlugin (PluginSpec ("augeas"));

	// std::cout << "Solution: ";
	// for (auto const & p : bb) std::cout << p.getName() << " ";
	// std::cout << std::endl;

	EXPECT_TRUE (bb.validated ()) << "Reordering not successful?";
}

TEST (GTEST_DISABLE_ASAN (MountBackendBuilder), allSort)
{
	using namespace kdb;
	using namespace kdb::tools;
	try
	{
		Backend b;
		b.addPlugin (PluginSpec ("resolver"));
		b.addPlugin (PluginSpec ("glob"));
		b.addPlugin (PluginSpec ("keytometa"));
		b.addPlugin (PluginSpec ("augeas"));
		// b.addPlugin (PluginSpec ("type"));
		// b.addPlugin (PluginSpec ("validation"));
		// b.addPlugin (PluginSpec ("struct", KeySet(5, *Key("user:/module", KEY_END), KS_END)));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what () << std::endl;
		return;
	}

	std::vector<std::string> permutation = {
		"augeas", "glob", "keytometa", "resolver"
		// , "type", "validation"
	};

	do
	{
		// for (auto const & p : permutation) std::cout << p << " ";
		// std::cout << std::endl;

		MountBackendBuilder bb;
		bb.addPlugin (PluginSpec (permutation[0]));
		bb.addPlugin (PluginSpec (permutation[1]));
		bb.addPlugin (PluginSpec (permutation[2]));
		bb.addPlugin (PluginSpec (permutation[3]));
		// bb.addPlugin (PluginSpec (permutation[4]));
		// bb.addPlugin (PluginSpec (permutation[5]));
		// bb.addPlugin (PluginSpec (permutation[6]));

		// std::cout << "Solution: ";
		// for (auto const & p : bb) std::cout << p.getName() << " ";
		// std::cout << std::endl;
		EXPECT_TRUE (bb.validated ()) << "Reordering not successful?";
	} while (std::next_permutation (permutation.begin (), permutation.end ()));
}

TEST (MountBackendBuilder, resolveNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	try
	{
		Backend b;
		b.addPlugin (PluginSpec ("resolver"));
		b.addPlugin (PluginSpec ("line"));
		b.addPlugin (PluginSpec ("null"));
	}
	catch (std::exception const & e)
	{
		std::cout << "Plugin missing, abort test case: " << e.what () << std::endl;
		return;
	}
	MountBackendBuilder bb;
	bb.addPlugin (PluginSpec ("resolver"));
	EXPECT_FALSE (bb.validated ()) << "resolver+null should be missing";
	bb.addPlugin (PluginSpec ("line"));
	EXPECT_FALSE (bb.validated ()) << "null should be missing";
	bb.resolveNeeds ();
	EXPECT_TRUE (bb.validated ()) << "Did not add null automatically";
}


TEST (BackendBuilder, resolveDoubleNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["needs"] = "c v";
	mpd->data[PluginSpec ("c")]["provides"] = "v";
	mpd->data[PluginSpec ("resolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("resolver"));
	bb.addPlugin (PluginSpec ("a"));
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 3);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("resolver"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c"));
}

TEST (BackendBuilder, resolveDoubleNeedsVirtual)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["needs"] = "v c";
	mpd->data[PluginSpec ("c")]["provides"] = "v";
	mpd->data[PluginSpec ("resolver")]["provides"] = "resolver";
	EXPECT_EQ (mpd->lookupInfo (PluginSpec ("c"), "provides"), "v");
	EXPECT_EQ (mpd->lookupInfo (PluginSpec ("c", "v"), "provides"), "v");
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("resolver"));
	bb.addPlugin (PluginSpec ("a"));
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 3);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("resolver"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c", "v")) << "remember it was virtual";
}

TEST (BackendBuilder, doubleAddWithConf)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["needs"] = "v c";
	mpd->data[PluginSpec ("c")]["provides"] = "v";
	mpd->data[PluginSpec ("resolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("resolver"));
	bb.addPlugin (PluginSpec ("a"));
	bb.addPlugin (PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	bb.addPlugin (PluginSpec ("v", KeySet (2, *Key ("user:/vef", KEY_END), KS_END)));
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 4);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("resolver"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_EQ (bb.cbegin ()[3], PluginSpec ("c", "v", KeySet (2, *Key ("user:/vef", KEY_END), KS_END))) << "remember it was virtual";
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 4);
}


TEST (BackendBuilder, doubleAddWithConfVirtual)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["needs"] = "v c";
	mpd->data[PluginSpec ("c")]["provides"] = "v";
	mpd->data[PluginSpec ("noresolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("resolver"));
	bb.addPlugin (PluginSpec ("a"));
	bb.addPlugin (PluginSpec ("v", KeySet (2, *Key ("user:/vef", KEY_END), KS_END)));
	bb.addPlugin (PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 4);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("noresolver", "resolver"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c", "v", KeySet (2, *Key ("user:/vef", KEY_END), KS_END)));
	EXPECT_EQ (bb.cbegin ()[3], PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 4);
}

TEST (BackendBuilder, directPluginLoading)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["plugins"] = "x a=b";
	mpd->data[PluginSpec ("a")]["needs"] = "resolver";
	mpd->data[PluginSpec ("x")]["provides"] = "x";
	mpd->data[PluginSpec ("noresolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("a"));
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("a"));
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 3);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("x", KeySet (2, *Key ("user:/a", KEY_VALUE, "b", KEY_END), KS_END)));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("noresolver", "resolver"));
}

TEST (BackendBuilder, metadata)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("r")]["metadata"] = "rename/toupper";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needMetadata ("rename/toupper");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("r"));
}

TEST (BackendBuilder, metadataTwo)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("r1")]["metadata"] = "rename/toupper";
	mpd->data[PluginSpec ("r1")]["status"] = "unittest";
	mpd->data[PluginSpec ("r2")]["metadata"] = "rename/toupper rename/tolower";
	mpd->data[PluginSpec ("r2")]["status"] = "memleak";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needMetadata ("rename/toupper rename/tolower");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("r2"));
}

TEST (BackendBuilder, metadataTwoRev)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("r1")]["metadata"] = "rename/tolower"; // relevant
	mpd->data[PluginSpec ("r1")]["status"] = "unittest";
	mpd->data[PluginSpec ("r2")]["metadata"] = "rename/toupper rename/tolower";
	mpd->data[PluginSpec ("r2")]["status"] = "memleak";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needMetadata ("rename/tolower rename/toupper"); // order not relevant
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("r1"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("r2"));
}

TEST (BackendBuilder, manualNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needPlugin ("n");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
}


TEST (BackendBuilder, manualNeedsProvides)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needPlugin ("x");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n", "x"));
}


TEST (BackendBuilder, manualMultipleNeeds)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	mpd->data[PluginSpec ("y")]["provides"] = "z";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needPlugin ("n");
	bb.needPlugin ("n");
	bb.needPlugin ("x");
	bb.needPlugin ("x");
	bb.needPlugin ("y");
	bb.needPlugin ("y");
	bb.needPlugin ("z");
	bb.needPlugin ("z");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("y"));
}

TEST (BackendBuilder, manualMultipleNeedsSingleLine)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	mpd->data[PluginSpec ("y")]["provides"] = "z";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.needPlugin ("n n x x y y z z");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("y"));
}


TEST (BackendBuilder, manualRecommends)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.recommendPlugin ("n");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
}


TEST (BackendBuilder, manualNoRecommends)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.recommendPlugin ("n");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds (false);
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
}


TEST (BackendBuilder, manualRecommendsProvides)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.recommendPlugin ("x");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds (true);
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 1);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n", "x"));
}


TEST (BackendBuilder, manualMultipleRecommends)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	mpd->data[PluginSpec ("y")]["provides"] = "z";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.recommendPlugin ("n");
	bb.recommendPlugin ("n");
	bb.recommendPlugin ("x");
	bb.recommendPlugin ("x");
	bb.recommendPlugin ("y");
	bb.recommendPlugin ("y");
	bb.recommendPlugin ("z");
	bb.recommendPlugin ("z");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("y"));
}

TEST (BackendBuilder, manualMultipleRecommendsSingleLine)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("n")]["provides"] = "x";
	mpd->data[PluginSpec ("y")]["provides"] = "z";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.recommendPlugin ("n n x x y y z z");
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 0);
	bb.resolveNeeds ();
	ASSERT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("n"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("y"));
}

TEST (BackendBuilder, resolveDoubleRecommends)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("a")]["recommends"] = "c v";
	mpd->data[PluginSpec ("c")]["provides"] = "v";
	mpd->data[PluginSpec ("resolver")]["provides"] = "resolver";
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	bb.addPlugin (PluginSpec ("resolver"));
	bb.addPlugin (PluginSpec ("a"));
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 2);
	bb.resolveNeeds ();
	EXPECT_EQ (std::distance (bb.cbegin (), bb.cend ()), 3);
	EXPECT_EQ (bb.cbegin ()[0], PluginSpec ("resolver"));
	EXPECT_EQ (bb.cbegin ()[1], PluginSpec ("a"));
	EXPECT_EQ (bb.cbegin ()[2], PluginSpec ("c"));
}

static int checkconfLookup (ckdb::Key * errorKey ELEKTRA_UNUSED, ckdb::KeySet * config)
{
	ckdb::Key * k = ckdb::ksLookupByName (config, "/a", 0);
	if (k)
	{
		return 0;
	}
	return -1;
}

TEST (BackendBuilder, checkconfOkNoChange)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf1")]["provides"] = "test123";
	mpd->setCheckconfFunction (checkconfLookup);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	PluginSpec spec ("checkconf1");
	KeySet pluginConfig;
	Key a;
	a.setName ("user:/a");
	a.setString ("abc");
	pluginConfig.append (a);
	spec.appendConfig (pluginConfig);
	bb.addPlugin (spec);
}

TEST (BackendBuilder, checkconfNotOKmissing)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf3")]["c"] = "something";
	mpd->setCheckconfFunction (checkconfLookup);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	EXPECT_THROW (bb.addPlugin (PluginSpec ("checkconf3")), PluginConfigInvalid);
}

static int checkconfAppend (ckdb::Key * errorKey ELEKTRA_UNUSED, ckdb::KeySet * config)
{
	ckdb::ksAppendKey (config, ckdb::keyNew ("user:/b", KEY_VALUE, "test", KEY_END));
	return 1;
}

TEST (BackendBuilder, checkconfOkChanged)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf1")]["provides"] = "test123";
	mpd->setCheckconfFunction (checkconfAppend);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	PluginSpec spec ("checkconf1");
	KeySet pluginConfig;
	spec.appendConfig (pluginConfig);
	bb.addPlugin (spec);
	// we expect b to be added now
	spec = *bb.begin ();
	EXPECT_EQ (spec.getConfig ().get<std::string> ("user:/b"), "test");
}

static int checkconfDelete (ckdb::Key * errorKey ELEKTRA_UNUSED, ckdb::KeySet * config)
{
	ckdb::ksCopy (config, NULL);
	return 1;
}

TEST (BackendBuilder, checkconfOkRemovedPluginConfig)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf1")]["provides"] = "test123";
	mpd->setCheckconfFunction (checkconfDelete);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	PluginSpec spec ("checkconf1");
	KeySet pluginConfig;
	Key a;
	a.setName ("user:/a");
	a.setString ("abc");
	pluginConfig.append (a);
	spec.appendConfig (pluginConfig);
	bb.addPlugin (spec);
	// we expect a to be removed now
	spec = *bb.begin ();
	EXPECT_THROW (spec.getConfig ().get<std::string> ("user:/a"), KeyNotFoundException);
}

TEST (BackendBuilder, checkconfOkRemovedBackendConfig)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf1")]["provides"] = "test123";
	mpd->setCheckconfFunction (checkconfDelete);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	PluginSpec spec ("checkconf1");
	KeySet pluginConfig;
	spec.appendConfig (pluginConfig);
	KeySet backendConfig;
	Key b;
	b.setName ("system:/b");
	b.setString ("xyz");
	backendConfig.append (b);
	bb.setBackendConfig (backendConfig);
	bb.addPlugin (spec);
	// we expect b to be removed now
	spec = *bb.begin ();
	EXPECT_THROW (bb.getBackendConfig ().get<std::string> ("system:/b"), KeyNotFoundException);
}

static int checkconfAppendBackendConf (ckdb::Key * errorKey ELEKTRA_UNUSED, ckdb::KeySet * config)
{
	ckdb::ksAppendKey (config, ckdb::keyNew ("system:/a", KEY_VALUE, "abc", KEY_END));
	return 1;
}

TEST (BackendBuilder, checkconfOkAppendBackendConfig)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase> ();
	mpd->data[PluginSpec ("checkconf1")]["provides"] = "test123";
	mpd->setCheckconfFunction (checkconfAppendBackendConf);
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	PluginSpec spec ("checkconf1");
	KeySet pluginConfig;
	spec.appendConfig (pluginConfig);
	bb.addPlugin (spec);
	// we expect b to be added now
	spec = *bb.begin ();
	EXPECT_EQ (bb.getBackendConfig ().get<std::string> ("system:/a"), "abc");
}
