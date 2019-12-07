/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "helper/keyhelper.hpp"
#include "kdbmerge.h"
#include <import.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <modules.hpp>
#include <plugindatabase.hpp>
#include <toolexcept.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::helper;

ImportCommand::ImportCommand ()
{
}

int ImportCommand::execute (Cmdline const & cl)
{
	size_t argc = cl.arguments.size ();
	if (argc != 1 && argc != 2 && argc != 3)
	{
		throw invalid_argument ("need 1 to 3 arguments");
	}

	kdb::Key root = cl.createKey (0);
	if (!root.isValid ())
	{
		throw invalid_argument ("root key \"" + cl.arguments[0] + "\" is not a valid key name");
	}

	kdb::KeySet originalKeys;
	kdb.get (originalKeys, root);
	printWarnings (cerr, root, cl.verbose, cl.debug);

	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

	string file = "/dev/stdin";
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	if (cl.verbose) std::cout << "lookup provider for: " << format << endl;

	ModulesPluginDatabase pluginDatabase;
	PluginSpec provides = pluginDatabase.lookupProvides (format);

	if (cl.verbose) std::cout << "found provider: " << provides.getName () << endl;

	Modules modules;
	PluginPtr plugin = modules.load (provides.getName (), cl.getPluginsConfig ());

	kdb::Key errorKey (root);
	errorKey.setString (file);

	kdb::KeySet importedKeys;
	plugin->get (importedKeys, errorKey);

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);
	printError (cerr, errorKey, cl.verbose, cl.debug);

	if (cl.strategy == "validate")
	{
		kdb::KeySet toset = prependNamespace (importedKeys, cl.ns);
		originalKeys.cut (prependNamespace (root, cl.ns));
		originalKeys.append (toset);

		PluginPtr specPlugin = modules.load ("spec", cl.getPluginsConfig ());
		if (specPlugin->get (originalKeys, root) == -1)
		{
			printWarnings (cerr, root, cl.verbose, cl.debug);
			printError (cerr, errorKey, cl.verbose, cl.debug);
			return -1;
		}

		if (cl.verbose)
		{
			cout.setf (std::ios_base::showbase);
			std::cout << originalKeys << std::endl;
		}

		kdb.set (originalKeys, root);
		printWarnings (cerr, root, cl.verbose, cl.debug);
		return 0;
	}

	kdb::KeySet base = originalKeys.cut (root);
	importedKeys = importedKeys.cut (root);
	if (cl.withoutElektra)
	{
		kdb::KeySet baseCopy = base.dup ();
		kdb::Key systemElektra ("system/elektra", KEY_END);
		kdb::KeySet systemKeySet = baseCopy.cut (systemElektra);
		importedKeys.append (systemKeySet);
	}
	ckdb::Key * informationKey = ckdb::keyNew (0, KEY_END);
	ckdb::KeySet * resultNew = elektraMerge (base.getKeySet (), root.getKey (), importedKeys.getKeySet (), root.getKey (),
						 base.getKeySet (), root.getKey (), root.getKey (), 1, informationKey);
	int numberOfConflicts = elektraMergeGetConflicts (informationKey);
	keyDel (informationKey);
	int retVal;
	if (resultNew != NULL)
	{
		originalKeys.append (resultNew);
		kdb.set (originalKeys, root);
		printWarnings (cerr, root, cl.verbose, cl.debug);
		printError (cerr, root, cl.verbose, cl.debug);
		retVal = 0;
	}
	else
	{
		if (numberOfConflicts > 0)
		{
			retVal = 11;
		}
		else
		{
			retVal = 3;
		}
	}
	return retVal;
}

ImportCommand::~ImportCommand ()
{
}
