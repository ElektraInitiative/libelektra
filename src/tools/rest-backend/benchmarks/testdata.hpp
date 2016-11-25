/**
 * @file
 *
 * @brief test data generator functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_BENCHMARK_TESTDATA_HPP
#define ELEKTRA_REST_BENCHMARK_TESTDATA_HPP

#include <vector>

#include <model_entry.hpp>
#include <model_user.hpp>

namespace kdbrest
{

namespace benchmark
{

std::vector<model::Entry> createTestEntries (model::User user, int num, int numTags);
std::vector<model::User> createTestUsers (int num);
}
}

#endif
