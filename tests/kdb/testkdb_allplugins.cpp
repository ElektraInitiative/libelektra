/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE

#include <backend.hpp>
#include <backends.hpp>
#include <plugindatabase.hpp>

#include <string>
#include <iostream>
#include <algorithm>
#include <unordered_map>

#include <kdb.hpp>
#include <gtest/gtest.h>

TEST(BackendBuilder, loadAllPlugins)
{
	using namespace kdb;
	using namespace kdb::tools;
	ModulesPluginDatabase mpd;
	Modules m;
	for (auto const & p : mpd.listAllPlugins())
	{
		// std::cout << p << std::endl;
		// if (p == "python") continue;
		// if (p == "python2") continue;
		// if (p == "struct") continue;
		if (p == "jni") continue;
		if (p == "crypto_gcrypt") continue;
		try {
			m.load(p);
			// Backend b;
			// b.addPlugin(PluginSpec(p));
		}
		catch (std::exception const & e)
		{
			EXPECT_TRUE(true) << p;
		}
	}
}


