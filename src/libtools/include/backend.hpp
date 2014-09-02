/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
 * @brief A representation of the backend (= set of plugins) that can be
 * mounted.
 */
class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string name;
	std::string mp;

	Modules modules;
	kdb::KeySet config; // the global config, plugins might add something to it

	std::vector <Plugin*> plugins;
	void tryPlugin (std::string name);

public:
	Backend(std::string name = "", std::string mountpoint = "");
	~Backend();

	void addPlugin (std::string name);
	void checkFile (std::string file) const;
	void status (std::ostream & os) const;
	bool validated () const;
	void serialise (kdb::Key &rootKey, kdb::KeySet &ret);
};

std::ostream & operator<<(std::ostream & os, Backend const & b);

}

}

#endif
