/**
 * @file
 *
 * @brief Delegate definitions for the `cpptemplate` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPP_TEMPLATE_DELEGATE_HPP
#define ELEKTRA_CPP_TEMPLATE_DELEGATE_HPP

#include <kdberrors.h>
#include <kdbplugin.hpp>

namespace elektra
{

class CppTemplateDelegate
{
	using KeySet = kdb::KeySet;

public:
	explicit CppTemplateDelegate (KeySet config);
};

} // end namespace elektra

#endif
