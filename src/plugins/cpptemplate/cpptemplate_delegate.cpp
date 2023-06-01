/**
 * @file
 *
 * @brief Delegate implementation for the `cpptemplate` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./cpptemplate_delegate.hpp"

using kdb::Key;
using kdb::KeySet;

namespace elektra
{

/**
 * @brief This constructor creates a new delegate object used by the `cpptemplate` plugin
 *
 * @param config This key set contains configuration values provided by the `cpptemplate` plugin
 */
CppTemplateDelegate::CppTemplateDelegate (KeySet config)
{
	configuration = config;
}

/**
 * @brief This method returns the configuration of the plugin, prefixing key names with the name of `parent`.
 *
 *  This is only an example to show you how to use the delegate. You can add any method you want here and then call it in
 *  `cpptemplate.cpp` via `delegator::get (handle)->functionName(parameter1, parameter2, …)`.
 *
 * @param parent This key specifies the name this function adds to the stored configuration values.
 *
 * @return A key set storing the configuration values of the plugin
 */
kdb::KeySet CppTemplateDelegate::getConfig (Key const & parent)
{
	KeySet keys;

	for (auto configKey : configuration)
	{
		Key key{ parent.getName (), KEY_END };
		key.addBaseName (configKey.getBaseName ());
		if (configKey.isString ()) key.setString (configKey.getString ());
		keys.append (key);
	}

	return keys;
}

} // end namespace elektra
