/**
 * @file
 *
 * @brief Tests for the plugindatabase class and implementations of it
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <algorithm>
#include <iostream>

#include <internal/kdb/config.h>
#include <plugindatabase.hpp>

#include <gtest/gtest.h>


TEST (PluginVariantsDatabase, listAllPlugins)
{
	using namespace kdb;
	using namespace kdb::tools;

	KeySet conf (0, KS_END);
	PluginVariantDatabase db (conf);
	std::vector<std::string> plugins (db.listAllPlugins ());

	ModulesPluginDatabase refDb;
	std::vector<std::string> allPlugins (refDb.listAllPlugins ());

	ASSERT_EQ (allPlugins.size (), plugins.size ());

	for (auto & elem : allPlugins)
	{
		ASSERT_TRUE (std::find (plugins.begin (), plugins.end (), elem) != plugins.end ());
	}

	for (auto & elem : plugins)
	{
		ASSERT_TRUE (std::find (allPlugins.begin (), allPlugins.end (), elem) != allPlugins.end ());
	}
}

TEST (PluginVariantsDatabase, listAllPluginsWithDisabled)
{
	using namespace kdb;
	using namespace kdb::tools;

	KeySet conf (2, *Key ("system:/elektra/plugins/dump/disable", KEY_VALUE, "1", KEY_END),
		     *Key ("system:/elektra/plugins/simpleini/disable", KEY_VALUE, "1", KEY_END), KS_END);
	PluginVariantDatabase db (conf);
	std::vector<std::string> plugins (db.listAllPlugins ());

	ModulesPluginDatabase refDb;
	std::vector<std::string> allPlugins (refDb.listAllPlugins ());

	// only execute if plugins dump & simpleini is available
	if (std::find (allPlugins.begin (), allPlugins.end (), "dump") != allPlugins.end () &&
	    std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		ASSERT_EQ (allPlugins.size () - 2, plugins.size ());
		ASSERT_TRUE (std::find (plugins.begin (), plugins.end (), "dump") == plugins.end ());
		ASSERT_TRUE (std::find (plugins.begin (), plugins.end (), "simpleini") == plugins.end ());
	}
}

TEST (PluginVariantsDatabase, getPluginVariants)
{
	using namespace kdb;
	using namespace kdb::tools;

	KeySet conf (0, KS_END);
	PluginVariantDatabase db (conf);
	std::vector<std::string> allPlugins (db.listAllPlugins ());

	if (std::find (allPlugins.begin (), allPlugins.end (), "dump") != allPlugins.end ())
	{
		PluginSpec dump ("dump");
		std::vector<PluginSpec> dump_variants (db.getPluginVariants (dump));
		ASSERT_EQ (0, dump_variants.size ());
	}

#ifndef ENABLE_ASAN
	// ASAN reports memory leaks for the Augeas plugin on macOS: https://travis-ci.org/sanssecours/elektra/jobs/418524229
	if (std::find (allPlugins.begin (), allPlugins.end (), "augeas") != allPlugins.end ())
	{
		PluginSpec augeas ("augeas");
		std::vector<PluginSpec> augeas_variants (db.getPluginVariants (augeas));
		ASSERT_TRUE (augeas_variants.size () > 0);
	}
#endif
}
