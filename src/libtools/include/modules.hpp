/**
 * @file
 *
 * @brief Allows one to load plugins
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_MODULES_HPP
#define TOOLS_MODULES_HPP

#include <plugin.hpp>
#include <keyset.hpp>
#include <toolexcept.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Allows one to load plugins
 */
class Modules
{
public:
	Modules();
	~Modules();

	/**
	 * @return a new created plugin
	 */
	PluginPtr load(std::string const& pluginName);
	PluginPtr load(std::string const& pluginName, kdb::KeySet const& config);

private:
	KeySet modules;
};

}

}

#endif
