/**
 * @file
 *
 * @brief Delegate definitions for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPP_LEAF_DELEGATE_HPP
#define ELEKTRA_CPP_LEAF_DELEGATE_HPP

#include <kdberrors.h>
#include <kdbplugin.hpp>

namespace elektra
{

class LeafDelegate
{
	using CppKeySet = kdb::KeySet;

public:
	explicit LeafDelegate (CppKeySet config);
};

} // end namespace elektra

#endif
