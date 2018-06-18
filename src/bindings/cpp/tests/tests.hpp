/**
 * @file
 *
 * @brief Some common functions in use for testing framework
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDB_TESTS_HPP
#define KDB_TESTS_HPP

#include <kdb.hpp>
#include <key.hpp>
#include <keyset.hpp>

#include <cstdlib>
#include <cstring>
#include <exception>
#include <iostream>
#include <string>

#include <gtest/gtest.h>

using namespace std;
using namespace kdb;

#define succeed_if(x, y) ASSERT_TRUE (x) << y
#define exit_if_fail(x, y) ASSERT_TRUE (x) << y
#define succeed_if_same(x, y, message) ASSERT_EQ (x, y) << message

#endif
