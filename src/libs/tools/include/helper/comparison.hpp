/**
 * @file
 *
 * @brief Comparison helper functions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef COMPARISON_HPP_
#define COMPARISON_HPP_

#include <kdb.hpp>
#include <string>

namespace kdb
{

namespace tools
{

namespace helper
{

/**
 * Determines if two keys are equal based on their string value
 * If one of the two keys is null, false is returned
 *
 * @param k1 the first key to be compared
 * @param k2 the second key to be compared
 * @return true if both keys are not null and have an
 * equal string value, false otherwise
 */
bool keyDataEqual (const Key &, const Key &);

/**
 * Determines if two keys have equal metadata
 *
 * The keys are not const because their meta cursor is changed
 *
 * @param k1 the first key whose metadata should be compared
 * @param k2 the second key whose metadata should be compared
 * @return true if the keys have equal metadata, false otherwise
 */
bool keyMetaEqual (Key &, Key &);
} // namespace helper
} // namespace tools
} // namespace kdb

#endif /* COMPARISON_HPP_ */
