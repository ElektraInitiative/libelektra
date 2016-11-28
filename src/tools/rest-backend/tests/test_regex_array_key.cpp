/**
 * @file
 *
 * @brief tests for the configformat model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <regex>

#include <gtest/gtest.h>

#include <service.hpp>

/**
 * TESTS for kdbrest::models::ConfigFormat
 */

TEST (kdbrestRegexArrayKeyTests, TestAllCombinations)
{

	std::regex regex (kdbrest::service::REGEX_CHECK_KEY_IS_ARRAY);

	ASSERT_FALSE (std::regex_match ("firstpart", regex));
	ASSERT_FALSE (std::regex_match ("firstpart.second", regex));
	ASSERT_FALSE (std::regex_match ("firstpart.second.third", regex));

	ASSERT_TRUE (std::regex_match ("#0", regex));
	ASSERT_TRUE (std::regex_match ("#5", regex));
	ASSERT_TRUE (std::regex_match ("#_12", regex));
	ASSERT_TRUE (std::regex_match ("#_2038", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#0.with.trailing.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#0.and.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#_12.and.path", regex));
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
