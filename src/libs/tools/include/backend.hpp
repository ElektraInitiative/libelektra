/**
 * @file
 *
 * @brief Implements a way to deal with a backend
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_HPP
#define TOOLS_BACKEND_HPP

#include <plugins.hpp>
#include <modules.hpp>
#include <toolexcept.hpp>

#include <ostream>
#include <string>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief A low-level representation of the backend (= set of plugins) that can be mounted.
 *
 * To build a backend, you should prefer BackendBuilder, which automatically fixes
 * ordering and allows us to remove plugins.
 */
class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string mp; // empty or valid canonified mountpoint
	std::string configFile; // empty or valid configuration file

	Modules modules;
	kdb::KeySet config; // the global config, plugins might add something to it

	std::vector <Plugin*> plugins;
	void tryPlugin (std::string name, KeySet pluginConf);

public:
	Backend();
	~Backend();

	void setMountpoint (Key mountpoint, KeySet mountConf);
	void setBackendConfig (KeySet const & ks);
	void addPlugin (std::string name, KeySet pluginConf = KeySet());
	void useConfigFile (std::string file);
	void status (std::ostream & os) const;
	bool validated () const;
	void serialize (kdb::KeySet &ret);

	std::string getMountpoint() const;
	std::string getConfigFile() const;
};

inline std::string Backend::getMountpoint() const
{
	return mp;
}

inline std::string Backend::getConfigFile() const
{
	return configFile;
}

std::ostream & operator<<(std::ostream & os, Backend const & b);

}

}

#endif
