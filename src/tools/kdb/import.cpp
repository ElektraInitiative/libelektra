/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <import.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <modules.hpp>
#include <plugindatabase.hpp>
#include <toolexcept.hpp>

#include <iostream>

#include <mergehelper.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;

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

	Key root = cl.createKey (0, false);
	if (!root.isValid ())
	{
		throw invalid_argument ("root key \"" + cl.arguments[0] + "\" is not a valid key name");
	}
	if (root.getNamespace () == kdb::ElektraNamespace::CASCADING)
	{
		cerr << "Aborting: Specify a namespace for importing." << endl;
		return 2;
	}


	KeySet originalKeys;
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

	Key errorKey (root);
	errorKey.setString (file);

	KeySet importedKeys;
	plugin->get (importedKeys, errorKey);

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);
	printError (cerr, errorKey, cl.verbose, cl.debug);

	KeySet base = originalKeys.cut (root);
	importedKeys = importedKeys.cut (root);
	if (cl.withoutElektra)
	{
		KeySet baseCopy = base.dup ();
		Key systemElektra ("system:/elektra", KEY_END);
		KeySet systemKeySet = baseCopy.cut (systemElektra);
		importedKeys.append (systemKeySet);
	}

	ThreeWayMerge merger;
	MergeHelper helper;

	helper.configureMerger (cl, merger);
	MergeResult result = merger.mergeKeySet (
		MergeTask (BaseMergeKeys (base, root), OurMergeKeys (base, root), TheirMergeKeys (importedKeys, root), root));

	helper.reportResult (cl, result, cout, cerr);

	int ret = -1;
	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			cout << "The merged keyset with strategy " << cl.strategy << " is:" << endl;
			cout << result.getMergedKeys ();
		}

		KeySet resultKeys = result.getMergedKeys ();
		originalKeys.append (resultKeys);
		kdb.set (originalKeys, root);
		ret = 0;

		printWarnings (cerr, root, cl.verbose, cl.debug);
		printError (cerr, root, cl.verbose, cl.debug);
	}

	return ret;
}

ImportCommand::~ImportCommand ()
{
}
