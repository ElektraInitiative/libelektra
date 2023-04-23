/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
	Key k = cl.createKey (0);
	Key parentKey = cl.getParentKey (cl.createKey (0));
	string metaname = cl.arguments[1];

	KeySet conf;
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	k = conf.lookup (k);

	if (!k)
	{
		cerr << "Key not found" << endl;
		return 11;
	}

	if (!k.getMeta<const Key> (metaname))
	{
		cerr << "Metakey not found" << endl;
		return 12;
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
