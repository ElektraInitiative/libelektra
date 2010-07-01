#ifndef PLUGINS_HPP
#define PLUGINS_HPP

#include <plugin.hpp>

#include <vector>
#include <string>

#include <kdb>

class Plugins
{
protected:
	std::vector<Plugin *> plugins;

	kdb::KeySet ret;

	std::vector <std::string> alreadyProvided;
	int nrStoragePlugins;

public:
	Plugins () :
		plugins (10),
		nrStoragePlugins (0)
	{}

	bool checkStorage (Plugin &plugin);
	bool checkInfo (Plugin &plugin);
};

class GetPlugins : public Plugins
{
public:
	/**
	 * Returns true if GetPlugins are valid afterwards.
	 *
	 * Will throw an exception if plugin could not
	 * be added.
	 */
	bool addPlugin (Plugin &plugin);
	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

class SetPlugins : public Plugins
{
public:
	bool addPlugin (Plugin &plugin);
};

class ErrorPlugins : public Plugins
{
public:
	bool addPlugin (Plugin &plugin);
};

#endif
