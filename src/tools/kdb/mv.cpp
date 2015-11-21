/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <mv.hpp>

#include <kdb.hpp>
#include <rename.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

MvCommand::MvCommand()
{}

Key baseName(Key key1, Key key2)
{
	if (key1.isBelowOrSame(key2)) return key2;
	if (key2.isBelowOrSame(key1)) return key1;

	if (key1.getNamespace() == key2.getNamespace()) return Key(key1.getNamespace(), KEY_END);

	return Key("/", KEY_END);
}

int MvCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 2)
	{
		throw invalid_argument("wrong number of arguments, 2 needed");
	}

	KeySet conf;
	Key sourceKey(cl.arguments[0], KEY_END);
	if (!sourceKey.isValid())
	{
		throw invalid_argument("Source given is not a valid keyname");
	}

	Key destKey(cl.arguments[1], KEY_END);
	if (!destKey.isValid())
	{
		throw invalid_argument("Destination given is not a valid keyname");
	}
	string newDirName = cl.arguments[1];

	Key root = baseName(sourceKey, destKey);
	if (cl.verbose) std::cout << "using common basename: " << root.getName() << std::endl;
	kdb.get(conf, root);
	KeySet tmpConf = conf;
	KeySet oldConf;

	oldConf.append (tmpConf.cut(sourceKey));

	KeySet newConf;

	Key k;
	oldConf.rewind();
	std::string sourceName = sourceKey.getName();
	if (cl.recursive)
	{
		while ((k = oldConf.next()))
		{
			newConf.append(rename_key(k, sourceName, newDirName, cl.verbose));
		}
	}
	else
	{
		// just rename one key
		k = oldConf.next();
		if (!k)
		{
			cerr << "Single key to move not found\n";
			return 1;
		}
		if (k != sourceKey)
		{
			cerr << "First key found " << k.getName()
			     << " does not exactly match given key " << sourceKey.getName()
			     << ", aborting (use -r to move hierarchy)\n";
			return 1;
		}
		newConf.append(rename_key(k, sourceName, newDirName, cl.verbose));
	}
	newConf.append(tmpConf); // these are unrelated keys
	// drop the original configuration

	newConf.rewind();
	if (cl.verbose)
	{
		cout << "Will write out:" << endl;
		cout << newConf;
	}

	kdb.set(newConf, root);
	printWarnings(cerr, root);

	return 0;
}

MvCommand::~MvCommand()
{}
