/**
 * @file
 *
 * @brief Implementation of get/set and error plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGINS_HPP
#define TOOLS_PLUGINS_HPP

#include <plugin.hpp>
#include <toolexcept.hpp>

#include <map>
#include <string>
#include <vector>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

struct Slot
{
	Plugin * value;
	Slot * next;
};

/**
 * @brief A collection of plugins (either get, set or error)
 */
class Plugins
{
protected:
	std::vector<Slot *> plugins;

	std::vector<std::string> needed;
	std::vector<std::string> recommended;
	std::vector<std::string> alreadyProvided;
	std::vector<std::string> alreadyConflict;

	int nrStoragePlugins;
	int nrResolverPlugins;

	std::map<std::string, int> placementInfo;

	void addPluginToSlot (Plugin * plugin, std::string which);

public:
	Plugins ();
	virtual ~Plugins ();

	/** Add needed, provided and recommend information */
	void addInfo (Plugin & plugin);
	void addPlugin (Plugin & plugin, std::string which);

	/** Validate needed, and provided information.
	 * (Recommended ignored, @see getRecommendedMissing(),
	 * @see getNeededMissing() */
	bool validateProvided () const;
	std::vector<std::string> getNeededMissing () const;
	std::vector<std::string> getRecommendedMissing () const;

	bool checkPlacement (Plugin & plugin, std::string which);
	void checkStorage (Plugin & plugin);
	void checkResolver (Plugin & plugin);
	void checkOrdering (Plugin & plugin);
	void checkConflicts (Plugin & plugin);
};

/**
 * @brief Plugins to get configuration
 */
class GetPlugins : private Plugins
{
public:
	/**
	 * Returns true if GetPlugins are valid afterwards.
	 *
	 * Will throw an exception if plugin could not
	 * be added.
	 */
	void tryPlugin (Plugin & plugin);
	void addPlugin (Plugin & plugin);
	bool validated () const;

	void serialise (kdb::Key & baseKey, kdb::KeySet & ret);
};


/**
 * @brief Plugins to set configuration
 */
class SetPlugins : private Plugins
{
public:
	void tryPlugin (Plugin & plugin);
	void addPlugin (Plugin & plugin);
	bool validated () const;

	void serialise (kdb::Key & baseKey, kdb::KeySet & ret);
};

/**
 * @brief Plugins to handle errors during configuration access
 */
class ErrorPlugins : private Plugins
{
public:
	void status (std::ostream & os) const;

	void tryPlugin (Plugin & plugin);
	void addPlugin (Plugin & plugin);
	bool validated () const;

	void serialise (kdb::Key & baseKey, kdb::KeySet & ret);
};

/**
 * @brief Plugins to handle errors during configuration access
 */
class CommitPlugins : private Plugins
{
public:
	void status (std::ostream & os) const;

	void tryPlugin (Plugin & plugin);
	void addPlugin (Plugin & plugin);
	bool validated () const;

	void serialise (kdb::Key & baseKey, kdb::KeySet & ret);
};
} // namespace tools
} // namespace kdb

#endif
