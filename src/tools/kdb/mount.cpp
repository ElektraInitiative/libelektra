/**
 * @file
 *
 * @brief source file of mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backendbuilder.hpp>
#include <backends.hpp>
#include <cmdline.hpp>
#include <mount.hpp>

#include <keysetio.hpp>

#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include <elektra/kdbmodule.h>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

MountCommand::MountCommand ()
{
}


/**
 * @brief Output what currently is mounted
 */
void MountCommand::outputMtab (Cmdline const & cl)
{
	Backends::BackendInfoVector mtab = Backends::getBackendInfo (mountConf);
	bool all = cl.first && cl.second;
	char delim = '\n';
	if (cl.null)
	{
		delim = '\0';
	}
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (cl.first)
		{
			std::cout << it->path;
			if (all)
				std::cout << " on ";
			else
				std::cout << delim << std::flush;
		}

		if (cl.second)
		{
			std::cout << it->mountpoint;
			std::cout << delim << std::flush;
		}
	}
}

void MountCommand::processArguments (Cmdline const & cl)
{
	if (!cl.interactive && cl.arguments.size () == 1)
	{
		throw invalid_argument ("wrong number of arguments, 0 or more than 1 needed");
	}

	if (cl.interactive)
	{
		cout << "Welcome to interactive mounting" << endl;
		cout << "Note that nothing will be made persistent" << endl;
		cout << "until you say y at the very end of the mounting process" << endl;
		cout << endl;
	}
}

void MountCommand::buildBackend (Cmdline const & cl)
{
	MountBackendBuilder backend;

	Key mpk (mp, KEY_END);

	if (!mpk.isValid ())
	{
		throw invalid_argument (mp + " is not a valid mountpoint");
	}

	KeySet dupMountConf = mountConf.dup ();

	if (cl.force || cl.strategy != "preserve")
	{
		Key cutKey (Backends::mountpointsPath, KEY_END);
		cutKey.addBaseName (mpk.getName ());
		mountConf.cut (cutKey);
	}

	backend.setMountpoint (mpk, mountConf);

	backend.setBackendConfig (cl.getPluginsConfig ("system:/"));

	PluginSpec resolver (cl.resolver);
	if (cl.debug)
	{
		cout << "Trying to load the resolver plugin " << resolver.getName () << endl;
	}

	backend.addPlugin (PluginSpec (resolver));

	if (cl.interactive)
	{
		cout << endl;
		cout << "Enter a path to a file in the filesystem." << endl;
		cout << "The path must either not exist or be a file." << endl;
		cout << "For user or cascading mountpoints it must be a relative path." << endl;
		cout << "Then, the path will be resolved dynamically." << endl;
		cout << "Path: ";
		cin >> path;
	}
	else
	{
		path = cl.arguments[0];
	}

	backend.useConfigFile (path);

	if (!cl.quiet && path[0] == '/' && !(mpk.isSystem () || mpk.isSpec () || mpk.isCascading ()))
	{
		std::cout << "Note that absolute paths are still relative to their namespace (see `kdb plugin-info resolver`)."
			  << std::endl;
		std::cout << "Only system+spec mountpoints are actually absolute." << std::endl;
		std::cout << "Use `kdb file " << mp << "` to determine where the file(s) are." << std::endl;
		std::cout << "Use `-q` or use `kdb set /sw/elektra/kdb/#0/current/quiet 1` to suppress infos." << std::endl;
	}

	if (cl.debug)
	{
		cout << "Trying to add default plugins " << cl.plugins << endl;
	}

	backend.needPlugin ("storage");
	backend.needPlugin ("sync");
	backend.addPlugins (parseArguments (cl.plugins));

	if (cl.interactive)
	{
		cout << "Now enter a sequence of plugins you want in the backend" << endl;
		appendPlugins (backend);
	}
	else
	{
		const size_t nonPlugins = 2;
		backend.addPlugins (parseArguments (cl.arguments.begin () + nonPlugins, cl.arguments.end ()));
	}

	// Call it a day
	outputMissingRecommends (backend.resolveNeeds (cl.withRecommends));
	backend.serialize (mountConf);

	if (cl.strategy == "unchanged" && cl.debug)
	{
		cout << "The new configuration is:" << endl;
		std::cout << mountConf;
		std::cout << "------------------------" << std::endl;
		cout << "The configuration originally was:" << endl;
		std::cout << dupMountConf;
	}

	if (!cl.force && (cl.strategy == "unchanged" && mountConf != dupMountConf))
	{
		// throw error because it is not unchanged
		try
		{
			backend.setMountpoint (mpk, dupMountConf);
		}
		catch (MountpointAlreadyInUseException const & e)
		{
			throw MountpointAlreadyInUseException (
				std::string ("Requested unchanged mountpoint but mount would lead to changes\n") + e.what ());
		}
	}
}

void MountCommand::readPluginConfig (KeySet & pluginConfig)
{
	string keyName;
	string value;

	cout << "Enter the plugin configuration" << endl;
	cout << "Use '.' as Key name to finish" << endl;

	while (true)
	{
		cout << "Enter the Key name: ";
		cin >> keyName;

		if (keyName == ".") break;

		cout << "Enter the Key value: ";
		cin >> value;

		pluginConfig.append (Key ("user:/" + keyName, KEY_VALUE, value.c_str (), KEY_END));
	}
}

void MountCommand::appendPlugins (MountBackendInterface & backend)
{
	std::string pname;
	KeySet pluginConfig;
	cout << "First Plugin: ";
	cin >> pname;
	readPluginConfig (pluginConfig);

	while (pname != "." || !backend.validated ())
	{
		try
		{
			backend.addPlugin (PluginSpec (pname, pluginConfig));
			pluginConfig.clear ();
		}
		catch (PluginCheckException const & e)
		{
			cerr << "Could not add that plugin" << endl;
			cerr << e.what () << endl;
		}
		if (!backend.validated ())
			cout << "Not validated, try to add another plugin (. to abort)" << endl;
		else
			cout << "Enter . to finish entering plugins" << endl;

		cout << endl;
		cout << "Next Plugin: ";
		cin >> pname;

		if (pname != ".")
		{
			readPluginConfig (pluginConfig);
		}

		if (pname == "." && !backend.validated ())
		{
			std::ostringstream os;
			backend.status (os);
			throw CommandAbortException (os.str ());
		}
	}
}


/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int MountCommand::execute (Cmdline const & cl)
{
	readMountConf (cl);

	if (!cl.interactive && cl.arguments.empty ())
	{
		// no interactive mode, so lets output the mtab
		outputMtab (cl);
		return 0;
	}

	processArguments (cl);
	getMountpoint (cl);
	buildBackend (cl);
	askForConfirmation (cl);
	doIt ();

	return 0;
}

MountCommand::~MountCommand ()
{
}
