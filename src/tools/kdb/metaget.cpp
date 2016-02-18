/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <metaget.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

MetaGetCommand::MetaGetCommand ()
{
}

int MetaGetCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 2)
	{
		throw invalid_argument ("Need 2 arguments");
	}
	Key parentKey = cl.createKey (0);
	string metaname = cl.arguments[1];

	KeySet conf;
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey);

	Key k = conf.lookup (parentKey);

	if (!k)
	{
		cerr << "Key not found" << endl;
		return 1;
	}

	if (!k.getMeta<const Key> (metaname))
	{
		cerr << "Metakey not found" << endl;
		return 2;
	}

	cout << k.getMeta<string> (metaname);

	if (!cl.noNewline)
	{
		cout << endl;
	}

	return 0;
}

MetaGetCommand::~MetaGetCommand ()
{
}
