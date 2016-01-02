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
	SpecBackendBuilder bi = sr.backends [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
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
	SpecBackendBuilder bi = sr.backends [Key ("user/mp", KEY_END)];
	EXPECT_EQ (bi.nodes, 2);
}

