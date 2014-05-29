/**
 * \file
 *
 * \brief Allows to load plugins
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_MODULES_HPP
#define TOOLS_MODULES_HPP

#include <plugin.hpp>
#include <keyset.hpp>
#include <toolexception.hpp>

class Modules
{
public:
	Modules();
	~Modules();

	/**
	 * @return a new created plugin
	 */
	kdb::PluginPtr load(std::string const& pluginName);
	kdb::PluginPtr load(std::string const& pluginName, kdb::KeySet const& config);

private:
	kdb::KeySet modules;
};

#endif
