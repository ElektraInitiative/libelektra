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

class Backend;
class BackendInterface;
class PluginDatabase;

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

	typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	PluginDatabasePtr pluginDatabase;

	BackendFactory backendFactory;

private:
	void sort();

public:
	BackendBuilder();
	explicit BackendBuilder(BackendBuilderInit const & bbi = BackendBuilderInit());

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
	void addPlugins (PluginSpecVector const & plugin);
	void addPlugin (PluginSpec const & plugin);
	void remPlugin (PluginSpec const & plugin);
	void resolveNeeds();

	void fillPlugins(BackendInterface & b) const;
};

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
	static KeySet parsePluginArguments (std::string const & pluginArguments);
	static PluginSpecVector parseArguments (std::string const & cmdline);

	void setMountpoint (Key mountpoint, KeySet mountConf);
	std::string getMountpoint() const;

	void setBackendConfig (KeySet const & ks);

	void useConfigFile (std::string file);
	std::string getConfigFile() const;

	void serialize (kdb::KeySet &ret);

	void status (std::ostream & os) const;
	bool validated () const;
};

}

}

#endif
