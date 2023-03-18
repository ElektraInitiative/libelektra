/**
 * @file
 *
 * @brief Delegate definitions for the `kconfig` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPP_KCONFIG_DELEGATE_HPP
#define ELEKTRA_CPP_KCONFIG_DELEGATE_HPP

#include <elektra/kdb/errors.h>
#include <kdbplugin.hpp>

namespace elektra
{

class KconfigDelegate
{
	using KeySet = kdb::KeySet;
	using Key = kdb::Key;

	/** This key set stores the plugin configuration. */
	KeySet configuration; // For your own plugin you can remove this value and also add any other member you like here.

public:
	explicit KconfigDelegate (KeySet config);

	/**
	 * @brief This method returns the configuration of the plugin, prefixing key names with the name of `parent`.
	 *
	 *  This is only an example to show you how to use the delegate. You can add any method you want here and then call it in
	 *  `kconfig.cpp` via `delegator::get (handle)->functionName(parameter1, parameter2, …)`.
	 *
	 * @param parent This key specifies the name this function adds to the stored configuration values.
	 *
	 * @return A key set storing the configuration values of the plugin
	 */
	KeySet getConfig (Key const & prefix);
};

} // end namespace elektra

#endif
