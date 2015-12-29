/**
 * @file
 *
 * @brief Implementation of PluginDatabase(s)
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <plugindatabase.hpp>

#include <modules.hpp>

namespace kdb
{

namespace tools
{

class ModulesPluginDatabase::Impl
{
public:
	Impl () {}
	~Impl () {}
	Modules modules;
};

ModulesPluginDatabase::ModulesPluginDatabase () :
	impl(new ModulesPluginDatabase::Impl())
{}

ModulesPluginDatabase::~ModulesPluginDatabase ()
{}

std::string ModulesPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	PluginPtr plugin = impl->modules.load (spec.name, spec.config);
	return plugin->lookupInfo(which);
}

}

}

