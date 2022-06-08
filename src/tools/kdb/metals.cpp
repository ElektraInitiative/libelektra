/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <metals.hpp>

#include <iostream>

#include <cmdline.hpp>
#include <kdb.hpp>

using namespace kdb;
using namespace std;

MetaLsCommand::MetaLsCommand ()
{
}

int MetaLsCommand::execute (Cmdline const & cl)
{
	int ret = 0;
	if (cl.arguments.size () != 1)
	{
		throw invalid_argument ("1 argument required");
	}

	Key root = cl.createKey (0);

	kdb.get (ks, root);

	Key k = ks.lookup (root);

	if (k)
	{
		if (cl.verbose)
		{
			std::cout << "Got key " << k.getName () << std::endl;
		}

		ckdb::KeySet * metaKeys = ckdb::keyMeta (k.getKey ());

		for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
		{
			const Key & curMeta = ckdb::ksAtCursor (metaKeys, it);
			cout << curMeta.getName ().substr (sizeof ("meta:/") - 1);
			if (cl.null)
			{
				cout << '\0' << std::flush;
			}
			else
			{
				cout << endl;
			}
		}
	}
	else
	{
		std::cerr << "Did not find key" << std::endl;
		ret = 1;
	}

	printWarnings (cerr, root, cl.verbose, cl.debug);

	return ret;
}

MetaLsCommand::~MetaLsCommand ()
{
}
