/**
 * @file
 *
 * @brief This file contains the declaration of a tree walker function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_WALK_HPP
#define ELEKTRA_PLUGIN_YAWN_WALK_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <kdb.hpp>

#include <yaep.h>

// -- Function -----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This function walks a syntax tree calling methods of the given
 *        listener.
 *
 * @param listener This argument specifies the listener which this function
 *                 uses to convert the syntax tree to a key set.
 * @param root This variable stores the root of the tree this function visits.
 */
void walk (Listener & listener, yaep_tree_node const * root);

#endif // ELEKTRA_PLUGIN_YAWN_WALK_HPP
