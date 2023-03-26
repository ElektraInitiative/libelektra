/**
 * @file
 *
 * @brief Implements a way to build backends
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_BUILDER_HPP
#define TOOLS_BACKEND_BUILDER_HPP


#include <memory>
#include <set>
#include <vector>

#include <kdb.hpp>

#include <backend.hpp>
#include <plugin.hpp>
#include <plugindatabase.hpp>
#include <pluginspec.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Used as argument of constructor of *BackendBuilder
 *
 * Avoids the implementation of 5 Constructors for each of the
 * *BackendBuilder.
 */
class BackendBuilderInit
{
private:
	// typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	// -> moved to plugindatabase.hpp since it is used in public API
	PluginDatabasePtr pluginDatabase;

	BackendFactory backendFactory;

public:
	BackendBuilderInit ();
	explicit BackendBuilderInit (PluginDatabasePtr const & plugins);
	explicit BackendBuilderInit (BackendFactory const & bf);
	BackendBuilderInit (PluginDatabasePtr const & plugins, BackendFactory const & bf);
	BackendBuilderInit (BackendFactory const & bf, PluginDatabasePtr const & plugins);

	PluginDatabasePtr getPluginDatabase () const
	{
		return pluginDatabase;
	}

	BackendFactory getBackendFactory () const
	{
		return backendFactory;
	}
};

/**
 * @brief Highlevel interface to build a backend.
 *
 * Automatically reorders plugins and has different modes which Backend
 * should be built.
 */
class BackendBuilder : public BackendInterface
{
private:
	/// Defines order in which plugins should be added
	PluginSpecVector toAdd;

	/// Metadata to be added
	std::set<std::string> metadata;

	/// Needed plugins (not yet added)
	std::vector<std::string> neededPlugins;

	/// Recommended plugins (not yet added)
	std::vector<std::string> recommendedPlugins;

	// typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	// -> moved to plugindatabase.hpp since it is used in public API
	PluginDatabasePtr pluginDatabase;

	BackendFactory backendFactory;

private:
	void sort ();
	void collectNeeds (std::vector<std::string> & needs) const;
	void collectRecommends (std::vector<std::string> & recommends) const;
	void removeProvided (std::vector<std::string> & needs) const;
	void removeMetadata (std::set<std::string> & needsMetadata) const;

protected:
	KeySet backendConf;

public:
	explicit BackendBuilder (BackendBuilderInit const & bbi = BackendBuilderInit ());

	typedef PluginSpecVector::const_iterator const_iterator;

	const_iterator begin () const
	{
		return toAdd.begin ();
	}
	const_iterator end () const
	{
		return toAdd.end ();
	}
	const_iterator cbegin () const
	{
		return toAdd.begin ();
	}
	const_iterator cend () const
	{
		return toAdd.end ();
	}

	PluginDatabasePtr const & getPluginDatabase () const
	{
		return pluginDatabase;
	}

	BackendFactory const & getBackendFactory () const
	{
		return backendFactory;
	}

	~BackendBuilder ();

	void addPlugins (PluginSpecVector const & plugins)
	{
		for (auto const & plugin : plugins)
		{
			addPlugin (plugin);
		}
	}

	void addPlugin (PluginSpec const & plugin);
	void remPlugin (PluginSpec const & plugin);

	void needMetadata (std::string metadata);
	void needPlugin (std::string provider);
	std::vector<std::string> resolveNeeds (bool addRecommends = true);

	void recommendPlugin (std::string provider);

	void fillPlugins (BackendInterface & b) const;

	void setBackendConfig (KeySet const & ks);
	KeySet getBackendConfig ();
};

/**
 * @brief Build global plugins
 */
class GlobalPluginsBuilder : public BackendBuilder
{
public:
	explicit GlobalPluginsBuilder (BackendBuilderInit const & bbi = BackendBuilderInit ());
	static const char * const globalPluginsPath;
	void serialize (kdb::KeySet & ret);
};

/**
 * @brief High-level functionality to build a mountpoint
 *
 * will enforce resolver and storage to be present
 */
class MountBackendBuilder : public MountBackendInterface, public BackendBuilder
{
	Key mountpoint;

	/**
	 * Contains the keys of system:/elektra/mountpoints.
	 * It is needed to detect if a mountpoint already exists.
	 */
	KeySet mountConf;

	std::string configfile;

public:
	void addPlugin (PluginSpec const & spec)
	{
		return BackendBuilder::addPlugin (spec);
	}

	explicit MountBackendBuilder (BackendBuilderInit const & bbi = BackendBuilderInit ());

	void setMountpoint (Key mountpoint, KeySet mountConf);
	std::string getMountpoint () const;

	void setBackendConfig (KeySet const & ks);

	void useConfigFile (std::string file);
	std::string getConfigFile () const;

	void serialize (kdb::KeySet & ret);

	void status (std::ostream & os) const;
	bool validated () const;
};
} // namespace tools
} // namespace kdb

#endif
