/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf_delegate.hpp"

namespace elektra
{
using CppKeySet = kdb::KeySet;

/**
 * @brief This constructor creates a new delegate object used by the `leaf` plugin
 *
 * @param config This key set contains configuration values provided by the `leaf` plugin
 */
LeafDelegate::LeafDelegate (CppKeySet config ELEKTRA_UNUSED)
{
}
} // end namespace elektra
