/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <metals.hpp>

#include <iostream>

#include <cmdline.hpp>
#include <kdb.hpp>

using namespace kdb;
using namespace std;

MetaLsCommand::MetaLsCommand () {}

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

		k.rewindMeta ();
		while (const Key meta = k.nextMeta ())
		{
			cout << meta.getName ();
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

	printWarnings (cerr, root);

	return ret;
}

MetaLsCommand::~MetaLsCommand () {}
