/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <metals.hpp>

#include <iostream>

#include <kdb.hpp>
#include <cmdline.hpp>

using namespace kdb;
using namespace std;

MetaLsCommand::MetaLsCommand()
{}

int MetaLsCommand::execute (Cmdline const& cl)
{

	if (cl.arguments.size() != 1){
		throw invalid_argument("1 argument required");
	}

	Key root = cl.createKey(0);

	kdb.get(ks, root);

	Key k = ks.lookup(root);
	if (k)
	{
		k.rewindMeta();
		while (const Key meta = k.nextMeta())
		{
			cout << meta.getName();
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

	printWarnings(cerr, root);

	return 0;
}

MetaLsCommand::~MetaLsCommand()
{}
