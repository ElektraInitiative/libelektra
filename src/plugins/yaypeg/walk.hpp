/**
 * @file
 *
 * @brief This file contains the declaration of a tree walker function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_WALK_HPP
#define ELEKTRA_PLUGIN_YAYPEG_WALK_HPP

// -- Imports ------------------------------------------------------------------

#include "listener.hpp"

#define TAO_PEGTL_NAMESPACE yaypeg

#include <tao/pegtl/contrib/parse_tree.hpp>

// -- Function -----------------------------------------------------------------

namespace yaypeg
{

/**
 * @brief This function walks a tree calling methods of the given listener.
 *
 * @param listener This argument specifies the listener which this function
 *                 uses to convert the tree to a key set.
 * @param root This variable stores the root of the tree this function visits.
 */
void walk (Listener & listener, tao::TAO_PEGTL_NAMESPACE::parse_tree::node const & root);

} // namespace yaypeg

#endif // ELEKTRA_PLUGIN_YAYPEG_WALK_HPP
