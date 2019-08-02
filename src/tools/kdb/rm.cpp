/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <rm.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

RemoveCommand::RemoveCommand ()
{
}

int RemoveCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("1 argument required");

	KeySet conf;
	Key x = cl.createKey (0);

	kdb.get (conf, x);

	KeySet savedKeys;

	if (cl.withoutElektra)
	{
		Key systemElektra ("system/elektra", KEY_END);
		savedKeys = conf.cut (systemElektra);
	}

	if (!cl.recursive)
	{
		Key f = conf.lookup (x, KDB_O_POP);

		if (!f)
		{
			cerr << "Did not find the key" << endl;
			return cl.force ? 0 : 1;
		}
	}
	else
	{
		// do recursive removing
		KeySet ks = conf.cut (x);

		if (ks.size () == 0)
		{
			cerr << "Did not find any key" << endl;
			return cl.force ? 0 : 1;
		}
	}

	conf.append (savedKeys);

	kdb.set (conf, x);

	return 0;
}

RemoveCommand::~RemoveCommand ()
{
}
