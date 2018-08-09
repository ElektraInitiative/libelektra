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
#include <kdbmacros.h>
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

#define exit_if_fail(expression, message)                                                                                                  \
	if (!(expression))                                                                                                                 \
	{                                                                                                                                  \
		cerr << __FILE__ << ":" << __LINE__ << ": Failure" << endl;                                                                \
		cerr << "Value of: " << ELEKTRA_STRINGIFY (expression) << endl;                                                            \
		cerr << "  Actual: false" << endl;                                                                                         \
		cerr << "Expected: true" << endl;                                                                                          \
		cerr << message << endl;                                                                                                   \
		exit (1);                                                                                                                  \
	}                                                                                                                                  \
	SUCCEED () << message

#define succeed_if_same(x, y, message) ASSERT_EQ (x, y) << message
#define compare_keyset(keySet1, keySet2) ASSERT_EQ (keySet1, keySet2) << "Key sets are not equal"

#endif
