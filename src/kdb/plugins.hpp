#ifndef PLUGINS_HPP
#define PLUGINS_HPP

#include <plugin.hpp>

#include <vector>
#include <string>
#include <map>

#include <kdb>

struct TooManyPlugins : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return  "Too many plugins!\n"
			"The plugin can't be positioned anymore.\n"
			"Try to reduce the number of plugins to get better performance.";
	}
};


struct Place
{
	int current;
	int max;

	Place () :
		current (-1),
		max(0)
	{}

	Place (int current, int max) :
		current (current),
		max (max)
	{}
};

class Plugins
{
protected:
	std::vector<Plugin *> plugins;

	kdb::KeySet ret;

	std::vector <std::string> needed;
	std::vector <std::string> recommended;
	std::vector <std::string> alreadyProvided;

	int nrStoragePlugins;
	int nrResolverPlugins;

	int revPostGet;

	std::map <std::string, Place> placementInfo;

public:
	Plugins ();

	/** Add needed, provided and recommend information */
	void addProvided (Plugin &plugin);
	void addPlugin (Plugin &plugin, std::string which);

	/** Validate needed, recommend and provided information */
	bool validateProvided();

	/** @return true if plugin should be ignored */
	bool checkPlacement (Plugin &plugin, std::string which);
	void checkStorage (Plugin &plugin);
	void checkResolver (Plugin &plugin);
	void checkInfo (Plugin &plugin);
};

class GetPlugins : private Plugins
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

class SetPlugins : private Plugins
{
public:
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated ();

	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

class ErrorPlugins : private Plugins
{
public:
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated ();

	void serialize (kdb::Key &baseKey, kdb::KeySet &ret);
};

#endif
