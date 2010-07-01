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
	int nrResolverPlugins;

public:
	Plugins () :
		plugins (10),
		nrStoragePlugins (0),
		nrResolverPlugins (0)
	{}

	void addProvided (Plugin &plugin);

	void checkProvided (Plugin &plugin);
	void checkStorage (Plugin &plugin);
	void checkResolver (Plugin &plugin);
	void checkInfo (Plugin &plugin);
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
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated ();

	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

class SetPlugins : public Plugins
{
public:
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated ();

	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

class ErrorPlugins : public Plugins
{
public:
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated ();

	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

#endif
