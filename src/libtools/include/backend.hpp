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
#include <toolexception.hpp>

#include <string>

#include <kdb.hpp>


class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string name;
	std::string mp;

	kdb::KeySet modules;
	kdb::KeySet config; // the global config, plugins might add something to it

	std::vector <Plugin*> plugins;

public:
	Backend(std::string name, std::string mountpoint);
	~Backend();

	void checkFile (std::string file);
	void tryPlugin (std::string name);
	void addPlugin ();
	bool validated ();
	void serialize (kdb::Key &rootKey, kdb::KeySet &ret);
};

#endif
