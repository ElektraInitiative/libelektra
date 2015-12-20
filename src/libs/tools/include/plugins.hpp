/**
 * @file
 *
 * @brief Implementation of get/set and error plugins
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGINS_HPP
#define TOOLS_PLUGINS_HPP

#include <plugin.hpp>
#include <toolexcept.hpp>

#include <vector>
#include <string>
#include <map>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

std::vector<std::string> listAllAvailablePlugins();


struct Place
{
	int current;
	int max;

	Place () :
		current (-1),
		max(0)
	{}

	Place (int current_, int max_) :
		current (current_),
		max (max_)
	{}
};

/**
 * @brief A collection of plugins (either get, set or error)
 */
class Plugins
{
protected:
	std::vector<Plugin *> plugins;

	std::vector <std::string> needed;
	std::vector <std::string> recommended;
	std::vector <std::string> alreadyProvided;
	std::vector <std::string> alreadyConflict;

	int nrStoragePlugins;
	int nrResolverPlugins;

	int revPostGet;

	std::map <std::string, Place> placementInfo;

public:
	Plugins ();

	/** Add needed, provided and recommend information */
	void addInfo (Plugin &plugin);
	void addPlugin (Plugin &plugin, std::string which);

	/** Validate needed, and provided information.
	 * (Recommended ignored, @see getRecommendedMissing(),
	 * @see getNeededMissing() */
	bool validateProvided() const;
	std::vector<std::string> getNeededMissing() const;
	std::vector<std::string> getRecommendedMissing() const;

	bool checkPlacement (Plugin &plugin, std::string which);
	void checkStorage (Plugin &plugin);
	void checkResolver (Plugin &plugin);
	void checkOrdering (Plugin &plugin);
	void checkConflicts (Plugin &plugin);
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
	bool validated () const;

	void serialise (kdb::Key &baseKey, kdb::KeySet &ret);
};

class SetPlugins : private Plugins
{
public:
	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated () const;

	void serialise (kdb::Key &baseKey, kdb::KeySet &ret);
};

class ErrorPlugins : private Plugins
{
public:
	void status (std::ostream & os) const;

	void tryPlugin (Plugin &plugin);
	void addPlugin (Plugin &plugin);
	bool validated () const;

	void serialise (kdb::Key &baseKey, kdb::KeySet &ret);
};

}

}

#endif
