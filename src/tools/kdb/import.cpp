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

	Key root = cl.createKey (0);
	if (!root.isValid ())
	{
		throw invalid_argument ("root key \"" + cl.arguments[0] + "\" is not a valid key name");
	}

	KeySet originalKeys;
	kdb.get (originalKeys, root);
	printWarnings (cerr, root, cl.verbose, cl.debug);

	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

	string file = "/dev/stdin";
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	Modules modules;
	PluginPtr plugin = modules.load (format, cl.getPluginsConfig ());

	Key errorKey (root);
	errorKey.setString (file);

	KeySet importedKeys;
	plugin->get (importedKeys, errorKey);

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);
	printError (cerr, errorKey, cl.verbose, cl.debug);

	if (cl.strategy == "validate")
	{
		KeySet toset = prependNamespace (importedKeys, cl.ns);
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

	KeySet base = originalKeys.cut (root);
	importedKeys = importedKeys.cut (root);
	if (cl.withoutElektra)
	{
		KeySet baseCopy = base.dup ();
		Key systemElektra ("system/elektra", KEY_END);
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
