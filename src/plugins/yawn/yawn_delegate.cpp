/**
 * @file
 *
 * @brief Delegate implementation for the `yawn` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yawn_delegate.hpp"

using kdb::Key;
using kdb::KeySet;

namespace elektra
{

/**
 * @brief This constructor creates a new delegate object used by the `yawn` plugin
 *
 * @param config This key set contains configuration values provided by the `yawn` plugin
 */
YawnDelegate::YawnDelegate (KeySet config)
{
	configuration = config;
}

/**
 * @brief This method returns the configuration of the plugin, prefixing key names with the name of `parent`.
 *
 *  This is only an example to show you how to use the delegate. You can add any method you want here and then call it in
 *  `yawn.cpp` via `delegator::get (handle)->functionName(parameter1, parameter2, …)`.
 *
 * @param parent This key specifies the name this function adds to the stored configuration values.
 *
 * @return A key set storing the configuration values of the plugin
 */
kdb::KeySet YawnDelegate::getConfig (Key const & parent)
{
	KeySet keys{ 0, KS_END };

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
