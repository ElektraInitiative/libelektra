/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <metaremove.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

MetaRemoveCommand::MetaRemoveCommand ()
{
}

MetaRemoveCommand::~MetaRemoveCommand ()
{
}

int MetaRemoveCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 2)
	{
		throw invalid_argument ("Need 2 arguments");
	}
	Key parentKey = cl.createKey (0);
	string metaname = cl.arguments[1];

	KeySet conf;
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	Key k = conf.lookup (parentKey);

	if (!k)
	{
		cerr << "Key not found" << endl;
		return 11;
	}

	k.delMeta (metaname);

	kdb.set (conf, parentKey);

	return 0;
}
