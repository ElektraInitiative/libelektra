/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <mv.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <rename.hpp>

#include <helper/keyhelper.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

MvCommand::MvCommand ()
{
}

int MvCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 2)
	{
		throw invalid_argument ("wrong number of arguments, 2 needed");
	}

	KeySet conf;
	Key sourceKey = cl.createKey (0, false);

	Key destKey = cl.createKey (1, false);
	string newDirName = destKey.getName ();

	Key root = tools::helper::commonKeyName (sourceKey, destKey);
	if (cl.verbose) std::cout << "using common basename: " << root.getName () << std::endl;
	kdb.get (conf, root);
	KeySet tmpConf = conf;
	KeySet oldConf;

	oldConf.append (tmpConf.cut (sourceKey));
	std::string sourceName = sourceKey.getName ();

	if (!oldConf.size ())
	{
		std::cerr << "No key to copy found below '" << sourceName << "'" << std::endl;
		return 11;
	}

	KeySet newConf;

	Key k;
	oldConf.rewind ();

	if (cl.recursive)
	{
		while ((k = oldConf.next ()))
		{
			newConf.append (rename_key (k, sourceName, newDirName, cl.verbose));
		}
	}
	else
	{
		// just rename one key
		k = oldConf.next ();
		if (k != sourceKey)
		{
			cerr << "First key found " << k.getName () << " does not exactly match given key " << sourceKey.getName ()
			     << ", aborting (use -r to move hierarchy)\n";
			return 11;
		}
		newConf.append (rename_key (k, sourceName, newDirName, cl.verbose));
	}
	newConf.append (tmpConf); // these are unrelated keys
	// drop the original configuration

	newConf.rewind ();
	if (cl.verbose)
	{
		cout << "Will write out:" << endl;
		cout << newConf;
	}

	kdb.set (newConf, root);
	printWarnings (cerr, root, cl.verbose, cl.debug);

	return 0;
}

MvCommand::~MvCommand ()
{
}
