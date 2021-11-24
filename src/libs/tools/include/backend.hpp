/**
 * @file
 *
 * @brief Implements a way to deal with a backend
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_HPP
#define TOOLS_BACKEND_HPP

#include <backendparser.hpp>
#include <modules.hpp>
#include <plugin.hpp>
#include <plugins.hpp>
#include <pluginspec.hpp>
#include <toolexcept.hpp>

#include <deque>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Minimal interface to add plugins
 */
class BackendInterface
{
public:
	virtual void addPlugin (PluginSpec const & spec) = 0;
	virtual ~BackendInterface () = 0;
};

typedef std::unique_ptr<BackendInterface> BackendInterfacePtr;

/**
 * @brief Interface to serialize a backend
 */
class SerializeInterface
{
public:
	virtual void serialize (kdb::KeySet & ret) = 0;
	virtual ~SerializeInterface () = 0;
};

/**
 * @brief Interface to work with mountpoints (backends) for factory
 */
class MountBackendInterface : public BackendInterface, public SerializeInterface
{
public:
	virtual void status (std::ostream & os) const = 0;
	virtual bool validated () const = 0;

	virtual void setMountpoint (Key mountpoint, KeySet mountConf) = 0;
	virtual std::string getMountpoint () const = 0;

	virtual void setBackendConfig (KeySet const & ks) = 0;

	virtual void useConfigFile (std::string file) = 0;
	virtual std::string getConfigFile () const = 0;

	virtual ~MountBackendInterface () = 0;
};

typedef std::unique_ptr<MountBackendInterface> MountBackendInterfacePtr;

/**
 * @brief A low-level representation of the backend (= set of plugins) that can be mounted.
 *
 * To build a backend, you should prefer BackendBuilder, which automatically fixes
 * ordering and allows us to remove plugins.
 */
class Backend : public MountBackendInterface
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;
	CommitPlugins commitplugins;

	std::string mp;		// empty or valid canonified mountpoint
	std::string configFile; // empty or valid configuration file

	Modules modules;
	kdb::KeySet config; // the global config, plugins might add something to it

	// make sure plugins get closed before modules
	std::vector<PluginPtr> plugins;


private:
	void tryPlugin (PluginSpec const & spec);

public:
	Backend ();
	~Backend ();

	Backend (Backend const & other) = delete;
	Backend & operator= (Backend const & other) = delete;

	Backend (Backend && other);
	Backend & operator= (Backend && other);

	void setMountpoint (Key mountpoint, KeySet mountConf);
	void setBackendConfig (KeySet const & ks);
	void addPlugin (PluginSpec const & spec);
	void useConfigFile (std::string file);
	void status (std::ostream & os) const;
	bool validated () const;
	void serialize (kdb::KeySet & ret);

	std::string getMountpoint () const;
	std::string getConfigFile () const;
};

/**
 * @brief Factory for MountBackendInterface
 */
class BackendFactory
{
	std::string which;

public:
	explicit BackendFactory (std::string whichBackend) : which (whichBackend)
	{
	}

	/**
	 * @brief Create classes that implement MountBackendInterface
	 */
	MountBackendInterfacePtr create () const
	{
		if (which == "backend")
		{
			return MountBackendInterfacePtr (new Backend ());
		}
		throw NoSuchBackend (which);
	}
};

inline std::string Backend::getMountpoint () const
{
	return mp;
}

inline std::string Backend::getConfigFile () const
{
	return configFile;
}

std::ostream & operator<< (std::ostream & os, Backend const & b);

/**
 * @brief Adds plugins in a generic map
 */
class PluginAdder : public BackendInterface
{
protected:
	Modules modules;

	std::unordered_map<std::string, std::deque<std::shared_ptr<Plugin>>> plugins;

public:
	void addPlugin (PluginSpec const & spec);
};

/**
 * @brief Low level representation of global plugins
 */
class GlobalPlugins : public PluginAdder, public SerializeInterface
{
public:
	void serialize (kdb::KeySet & ret);
};

/**
 * @brief Backend for import/export functionality
 *
 * (only partly implemented)
 */
class ImportExportBackend : public PluginAdder
{

public:
	ImportExportBackend ()
	{
	}

	void status (std::ostream & os) const;
	void importFromFile (KeySet & ks, Key const & parentKey) const;
	void exportToFile (KeySet const & ks, Key const & parentKey) const;
};
} // namespace tools
} // namespace kdb

#endif
