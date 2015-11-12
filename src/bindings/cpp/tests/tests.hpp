/**Some common functions in use for testing framework*/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

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
