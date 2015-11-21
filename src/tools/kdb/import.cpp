/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <import.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>
#include <toolexcept.hpp>

#include <iostream>

#include <merging/threewaymerge.hpp>
#include <merging/metamergestrategy.hpp>
#include <mergehelper.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;

ImportCommand::ImportCommand()
{}

int ImportCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();
	if (argc != 1 && argc != 2 && argc != 3)
	{
		throw invalid_argument("need 1 to 3 arguments");
	}

	Key root (cl.arguments[0], KEY_END);
	if (!root.isValid())
	{
		throw invalid_argument ("root key \"" + cl.arguments[0] + "\" is not a valid key name");
	}

	KeySet originalKeys;
	kdb.get (originalKeys, root);
	KeySet base = originalKeys.cut (root);
	printWarnings (cerr, root);

	KeySet importedKeys;

	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

	string file = "/dev/stdin";
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	Modules modules;
	PluginPtr plugin = modules.load (format);

	Key errorKey (root);
	errorKey.setString (file);

	plugin->get(importedKeys, errorKey);
	importedKeys = importedKeys.cut(root);

	printWarnings (cerr, errorKey);
	printError (cerr, errorKey);

	ThreeWayMerge merger;
	MergeHelper helper;

	helper.configureMerger (cl, merger);
	MergeResult result = merger.mergeKeySet (
			MergeTask (BaseMergeKeys (base, root), OurMergeKeys (base, root),
					TheirMergeKeys (importedKeys, root), root));

	helper.reportResult (cl, result, cout, cerr);

	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			cout << "The merged keyset with strategy " << cl.strategy << " is:" << endl;
			cout << result.getMergedKeys();
		}

		KeySet resultKeys = result.getMergedKeys();
		originalKeys.append(resultKeys);
		kdb.set (originalKeys, root);
		return 0;
	}
	else
	{
		return -1;
	}
}

ImportCommand::~ImportCommand()
{}
