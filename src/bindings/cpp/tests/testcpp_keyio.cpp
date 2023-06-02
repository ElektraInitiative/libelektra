/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyio.hpp>

#include "./tests.hpp"

#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

TEST (keyio, out)
{
	Key k ("user:/hello", KEY_META, "abc", "", KEY_META, "def", "", KEY_END);
	std::stringstream ss;
	ss << k;
	EXPECT_EQ (ss.str (), "user:/hello");
}

TEST (keyio, in)
{
	Key k ("user:/hello", KEY_META, "abc", "", KEY_META, "def", "", KEY_END);
	Key k2;
	std::stringstream ss ("user:/hello");
	ss >> k2;
	EXPECT_EQ (k, k2);
}


TEST (keyio, outmeta)
{
	Key k ("user:/hello", KEY_META, "abc", "", KEY_META, "def", "", KEY_END);
	std::stringstream ss;
	ss.setf (std::ios_base::showbase);
	ss << k;
	EXPECT_EQ (ss.str (), "user:/hello meta:/abc meta:/def");
}

TEST (keyio, inmeta)
{
	Key k ("user:/hello", KEY_META, "abc", "", KEY_META, "def", "", KEY_END);
	Key k2;
	std::stringstream ss ("user:/hello meta:/abc meta:/def");
	ss.setf (std::ios_base::showbase);
	ss >> k2;
	EXPECT_EQ (k, k2);
}
