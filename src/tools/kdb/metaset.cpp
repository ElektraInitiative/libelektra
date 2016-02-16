/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <metaset.hpp>

#include <iostream>
#include <string>

#include <kdb.hpp>
#include <cmdline.hpp>

using namespace std;
using namespace kdb;

MetaSetCommand::MetaSetCommand()
{}

int MetaSetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 3)
	{
		throw invalid_argument ("Need 3 arguments");
	}
	string metaname = cl.arguments[1];

	Key parentKey = cl.createKey(0);
	string keyname = parentKey.getName();
	if (keyname[0] == '/')
	{
		// fix name for lookup
		keyname = "spec" + keyname;
		std::cout << "Using keyname " << keyname << std::endl;

		// fix k for kdb.set later
		parentKey.setName(keyname);
	}

	KeySet conf;
	kdb.get(conf, parentKey);
	Key k = conf.lookup(parentKey);

	if (!k)
	{
		k = Key(keyname, KEY_END);
		// k.setBinary(0, 0); // conceptually maybe better, but would have confusing "binary" metadata
		conf.append(k);
		if (cl.verbose) cout << "Creating key " << keyname << endl;
	}
	if (!k.isValid())
	{
		cout << "Could not create key" << endl;
		return 1;
	}

	std::string metavalue = cl.arguments[2];
	if (metaname == "atime" || metaname == "mtime" || metaname == "ctime")
	{
		stringstream str (metavalue);
		time_t t;
		str >> t;
		if (!str.good()) throw "conversion failure";
		k.setMeta<time_t> (metaname, t);
	} else {
		k.setMeta<string> (metaname, metavalue);
	}

	kdb.set(conf,parentKey);
	printWarnings(cerr,parentKey);

	return 0;
}

MetaSetCommand::~MetaSetCommand()
{}
