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

static int noKeyFound (bool verbose, bool force, std::string article)
{
	if (verbose || !force)
	{
		cerr << "Did not find " << article << " key" << endl;
	}
	return force ? 0 : 11;
}

int RemoveCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("1 argument required");

	KeySet conf;
	Key x = cl.createKey (0);

	if (x.getNamespace () == kdb::ElektraNamespace::CASCADING)
	{
		cerr << "Aborting: Specify a namespace for deleting a key." << endl;
		return 12;
	}

	Key parentKey = cl.getParentKey (x);

	kdb.get (conf, parentKey);

	KeySet savedKeys;

	if (cl.withoutElektra)
	{
		Key systemElektra ("system:/elektra", KEY_END);
		savedKeys = conf.cut (systemElektra);
	}

	if (!cl.recursive)
	{
		Key f = conf.lookup (x, KDB_O_POP);

		if (!f)
		{
			return noKeyFound (cl.verbose, cl.force, "the");
		}
	}
	else
	{
		// do recursive removing
		KeySet ks = conf.cut (x);

		if (ks.size () == 0)
		{
			return noKeyFound (cl.verbose, cl.force, "any");
		}
	}

	conf.append (savedKeys);

	kdb.set (conf, parentKey);

	return 0;
}

RemoveCommand::~RemoveCommand ()
{
}
