/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

#include <backend.hpp>
#include <backends.hpp>
#include <plugindatabase.hpp>

#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>

#include <gtest/gtest.h>
#include <kdb.hpp>
#include <kdbconfig.h>
#include <valgrind/valgrind.h>

#ifdef ENABLE_ASAN
#include <sanitizer/lsan_interface.h>
#endif

std::vector<std::string> getAllPlugins ()
{
	using namespace kdb;
	using namespace kdb::tools;
	ModulesPluginDatabase mpd;
	std::vector<std::string> plugins = mpd.listAllPlugins ();

	// The JNI and Ruby plugins cause segmentation faults
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "jni"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "ruby"), plugins.end ());
	bool removePluginsWithMemoryLeaks = false;

#ifdef ENABLE_ASAN
	// ASAN reports memory leaks for the Augeas plugin on macOS: https://travis-ci.org/sanssecours/elektra/jobs/418524229
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "augeas"), plugins.end ());
	removePluginsWithMemoryLeaks = true;
#endif
	if (RUNNING_ON_VALGRIND != 0)
	{
		removePluginsWithMemoryLeaks = true;
	}

	if (removePluginsWithMemoryLeaks)
	{

		std::vector<std::string> pluginsWithMemoryLeaks;

		for (auto plugin : plugins)
		{
			try
			{
#ifdef ENABLE_ASAN
#ifndef ASAN_NO_LEAK_SANITIZER_SUPPORT
				__lsan_disable ();
#endif
#endif
				auto status = mpd.lookupInfo (PluginSpec (plugin), "status");
#ifdef ENABLE_ASAN
#ifndef ASAN_NO_LEAK_SANITIZER_SUPPORT
				__lsan_enable ();
#endif
#endif
				if (status.find ("memleak")) pluginsWithMemoryLeaks.push_back (plugin);
			}
			catch (std::exception const & error)
			{
				std::cerr << "Unable to determine status of plugin “" << plugin << "”: " << error.what () << std::endl;
			}
		}

		for (auto plugin : pluginsWithMemoryLeaks)
		{
			plugins.erase (std::remove (plugins.begin (), plugins.end (), plugin), plugins.end ());
		}
	}
	return plugins;
}

class AllPlugins : public ::testing::TestWithParam<std::string>
{
protected:
};

TEST_P (AllPlugins, backend)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::string p = GetParam ();
	std::cout << p << std::endl;

	try
	{
		Backend b;
		b.addPlugin (PluginSpec (p));
	}
	catch (std::exception const & e)
	{
		EXPECT_TRUE (true) << p;
	}
}

TEST_P (AllPlugins, modules)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::string p = GetParam ();
	std::cout << p << std::endl;

	try
	{
		Modules m;
		m.load (p);
	}
	catch (std::exception const & e)
	{
		EXPECT_TRUE (true) << p;
	}
}


INSTANTIATE_TEST_SUITE_P (AllPlugins, AllPlugins, testing::ValuesIn (getAllPlugins ()));
