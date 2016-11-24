#ifndef ELEKTRA_REST_BENCHMARK_TESTDATA_HEADER_GUARD
#define ELEKTRA_REST_BENCHMARK_TESTDATA_HEADER_GUARD

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
