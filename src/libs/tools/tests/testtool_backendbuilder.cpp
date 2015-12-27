/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backends.hpp>
#include <backendbuilder.hpp>

#include <iostream>
#include <string>
#include <kdb.hpp>
#include <gtest/gtest.h>



TEST(BackendBuilder, basicAddRem)
{
	using namespace kdb;
	using namespace kdb::tools;
	BackendBuilder bb;
	bb.addPlugin(BackendBuilder::PluginSpec("resolver"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(BackendBuilder::PluginSpec("dump"));
	EXPECT_TRUE(bb.validated());

	bb.remPlugin(BackendBuilder::PluginSpec("dump"));
	EXPECT_FALSE(bb.validated());

	bb.addPlugin(BackendBuilder::PluginSpec("dump"));
	EXPECT_TRUE(bb.validated());
}

