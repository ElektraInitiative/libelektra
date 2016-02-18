/**
 * @file
 *
 * @brief Some common functions in use for testing framework
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

#define succeed_if(x, y)                                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ASSERT_TRUE (x) << y;                                                                                                      \
	} while (0)
#define exit_if_fail(x, y)                                                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ASSERT_TRUE (x) << y;                                                                                                      \
	} while (0)

#endif
