/**
 * @file
 *
 * @brief Implements a way to build backends
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_BUILDER_HPP
#define TOOLS_BACKEND_BUILDER_HPP


#include <set>
#include <memory>
#include <vector>

#include <kdb.hpp>

#include <plugin.hpp>
#include <backend.hpp>
#include <pluginspec.hpp>
#include <plugindatabase.hpp>

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
	typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	PluginDatabasePtr pluginDatabase;

	BackendFactory backendFactory;
public:
	BackendBuilderInit ();
	BackendBuilderInit (PluginDatabasePtr const & plugins);
	BackendBuilderInit (BackendFactory const & bf);
	BackendBuilderInit (PluginDatabasePtr const & plugins, BackendFactory const & bf);
	BackendBuilderInit (BackendFactory const & bf, PluginDatabasePtr const & plugins);

	PluginDatabasePtr getPluginDatabase() const
	{
		return pluginDatabase;
	}

	BackendFactory getBackendFactory() const
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
	std::set<std::string> metadata;

	typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	PluginDatabasePtr pluginDatabase;

	BackendFactory backendFactory;

private:
	void sort();
	void collectNeeds(std::vector<std::string> & needs) const;
	void removeProvided(std::vector<std::string> & needs) const;
	void removeMetadata(std::set<std::string> & needsMetadata) const;

public:
	BackendBuilder();
	explicit BackendBuilder(BackendBuilderInit const & bbi = BackendBuilderInit());

	// iterate over arguments to add plugins
	template <typename Iterator>
	void addPlugins (Iterator begin, Iterator end);

	typedef PluginSpecVector::const_iterator const_iterator;

	const_iterator begin() const { return toAdd.begin(); }
	const_iterator end() const { return toAdd.end(); }
	const_iterator cbegin() const { return toAdd.begin(); }
	const_iterator cend() const { return toAdd.end(); }

	PluginDatabasePtr const & getPluginDatabase() const
	{
		return pluginDatabase;
	}

	BackendFactory const & getBackendFactory() const
	{
		return backendFactory;
	}

	~BackendBuilder();

	void addPlugin (PluginSpec const & plugin);
	void remPlugin (PluginSpec const & plugin);

	void needMetadata (std::string metadata);
	void needPlugin (PluginSpec plugin);
	void resolveNeeds();

	void fillPlugins(BackendInterface & b) const;
};

/**
 * @brief High-level functionality to build a mountpoint
 */
class MountBackendBuilder : public MountBackendInterface, public BackendBuilder
{
	Key mountpoint;
	KeySet backendConf;
	KeySet mountConf;
	std::string configfile;
public:
	void addPlugin (PluginSpec const & spec)
	{
		return BackendBuilder::addPlugin(spec);
	}

	explicit MountBackendBuilder(BackendBuilderInit const & bbi = BackendBuilderInit());

	void setMountpoint (Key mountpoint, KeySet mountConf);
	std::string getMountpoint() const;

	void setBackendConfig (KeySet const & ks);
	KeySet getBackendConfig()
	{
		return backendConf;
	}

	void useConfigFile (std::string file);
	std::string getConfigFile() const;

	void serialize (kdb::KeySet &ret);

	void status (std::ostream & os) const;
	bool validated () const;
};

}

}

#endif
