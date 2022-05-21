/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <showmeta.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

ShowMetaCommand::ShowMetaCommand ()
{
}

int ShowMetaCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	Key root = cl.createKey (0);
	KeySet conf;
	kdb.get (conf, root);
	printWarnings (cerr, root, cl.verbose, cl.debug);

	Key k = conf.lookup (root);

	if (!k)
	{
		cerr << "Key not found" << endl;
		return 1;
	}

	KeySet metaKeys = ckdb::keyMeta (k.getKey ());
	for (const Key & curMeta : metaKeys)
	{
		cout << curMeta.getName ().substr (sizeof ("meta:/") - 1) << ": " << curMeta.getString () << endl;
	}

	return 0;
}

ShowMetaCommand::~ShowMetaCommand ()
{
}
