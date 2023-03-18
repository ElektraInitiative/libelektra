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
#include <internal/kdb/config.h>

#ifdef ENABLE_ASAN
#include <sanitizer/lsan_interface.h>
#endif

#include <internal/macros/utils.h>

ELEKTRA_UNUSED static bool isRunningWithValgrind (void)
{
	char * p = getenv ("LD_PRELOAD");
	if (p == nullptr) return 0;
	return (strstr (p, "/valgrind/") != nullptr || strstr (p, "/vgpreload") != nullptr);
}

// use extern "C" and non-static function to avoid name mangling
// we need a non-mangled name to use in the valgrind.supression file
extern "C" bool testkdb_allplugins_isMemleak (const kdb::tools::ModulesPluginDatabase & mpd, const std::string & plugin)
{
	try
	{
#if defined(ENABLE_ASAN) && !defined(ASAN_NO_LEAK_SANITIZER_SUPPORT)
		__lsan_disable ();
#endif
		auto status = mpd.lookupInfo (kdb::tools::PluginSpec (plugin), "status");
#if defined(ENABLE_ASAN) && !defined(ASAN_NO_LEAK_SANITIZER_SUPPORT)
		__lsan_enable ();
#endif
		bool memleak = status.find ("memleak") != std::string::npos;
		return memleak;
	}
	catch (std::exception const & error)
	{
		std::cerr << "Unable to determine status of plugin “" << plugin << "” will assume memleak: " << error.what () << std::endl;
		return true;
	}
}

std::vector<std::string> getAllPlugins ()
{
	using namespace kdb;
	using namespace kdb::tools;
	ModulesPluginDatabase mpd;
	std::vector<std::string> plugins = mpd.listAllPlugins ();

	// The JNI and Ruby plugins cause segmentation faults
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "jni"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "ruby"), plugins.end ());

#ifdef ENABLE_ASAN
	bool memcheck = true;
#else
	bool memcheck = isRunningWithValgrind ();
#endif

	if (memcheck)
	{
		std::cout << "running memcheck" << std::endl;
#if defined(__APPLE__)
		// ASAN reports memory leaks for the Augeas plugin on macOS: https://travis-ci.org/sanssecours/elektra/jobs/418524229
		plugins.erase (std::remove (plugins.begin (), plugins.end (), "augeas"), plugins.end ());
#endif

		std::vector<std::string> filtered;
		std::copy_if (plugins.begin (), plugins.end (), std::back_inserter (filtered),
			      [&] (const std::string & plugin) { return !testkdb_allplugins_isMemleak (mpd, plugin); });

		std::cout << "found " << plugins.size () << " plugins " << filtered.size () << " without memleak" << std::endl;
		return filtered;
	}
	else
	{
		std::cout << "found " << plugins.size () << " plugins" << std::endl;
		return plugins;
	}
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
