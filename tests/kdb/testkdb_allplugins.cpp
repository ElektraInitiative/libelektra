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

std::vector<std::string> getAllPlugins ()
{
	using namespace kdb;
	using namespace kdb::tools;
	ModulesPluginDatabase mpd;
	std::vector<std::string> plugins = mpd.listAllPlugins ();

	// remove known problems
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "process"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "haskell"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "xerces"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "ruby"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "jni"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "crypto_gcrypt"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "crypto_openssl"), plugins.end ());
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "crypto_botan"), plugins.end ());

	// Valgrind reports memory leaks for the `semlock` plugin on Debian Unstable: http://issues.libelektra.org/2113
	plugins.erase (std::remove (plugins.begin (), plugins.end (), "semlock"), plugins.end ());

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
	// std::cout << p << std::endl;

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
	// std::cout << p << std::endl;

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


INSTANTIATE_TEST_CASE_P (AllPlugins, AllPlugins, testing::ValuesIn (getAllPlugins ()), );
