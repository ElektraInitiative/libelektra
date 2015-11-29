/**
 * @file
 *
 * @brief Some common functions in use for testing framework
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDB_TESTS_HPP
#define KDB_TESTS_HPP

#include <key.hpp>
#include <keyset.hpp>
#include <kdb.hpp>

#include <exception>
#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>

#include <gtest/gtest.h>

using namespace std;
using namespace kdb;

#define succeed_if(x,y) do {ASSERT_TRUE(x) << y;} while (0)
#define exit_if_fail(x,y) do {ASSERT_TRUE(x) << y;} while (0)

#endif
