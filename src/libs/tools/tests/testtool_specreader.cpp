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

class MockPluginDatabase : public kdb::tools::PluginDatabase
{
public:
	mutable std::unordered_map <kdb::tools::PluginSpec, std::unordered_map<std::string,std::string>> data;

	std::string lookupInfo(kdb::tools::PluginSpec const & spec, std::string const & which) const
	{
		return data[spec][which];
	}

	kdb::tools::PluginSpec lookupProvides (std::string const & which) const
	{
		for (auto const & plugin : data)
		{
			if (plugin.first.name == which)
			{
				return plugin.first;
			}

			if (lookupInfo (plugin.first, "provides") == which)
			{
				return plugin.first;
			}
		}

		throw kdb::tools::NoPlugin("No plugin " + which + " could be found");
	}
};

TEST(SpecReader, withDatabase)
{
	using namespace kdb;
	using namespace kdb::tools;
	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data [PluginSpec("a")] ["ordering"] = "d";
	mpd->data [PluginSpec("b")] ["ordering"] = "d";
	mpd->data [PluginSpec("c")] ["ordering"];
	SpecReader sr(mpd);
	sr.readSpecification(KeySet(5,
				*Key ("user", KEY_END),
				*Key ("user/mp", KEY_META, "mountpoint", "file.ini", KEY_END),
				*Key ("user/mp/below", KEY_META, "needs", "something", KEY_END),
				KS_END));
	SpecBackendBuilder bi = sr.backends [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
}

