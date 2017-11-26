/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cp.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <rename.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

CpCommand::CpCommand ()
{
}

namespace
{
void copySingleKey (Cmdline const & cl, Key const & rk, KeySet & tmpConf, KeySet & newConf)
{
	if (cl.force)
	{
		tmpConf.lookup (rk, KDB_O_POP);
	}
	else if (tmpConf.lookup (rk))
	{
		throw CommandAbortException (
			std::string ("Copy will have no effect, because " + rk.getName () + " already exists").c_str (), 11);
	}
	newConf.append (rk);
}
}

int CpCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 2)
	{
		throw invalid_argument ("wrong number of arguments, 2 needed");
	}

	KeySet conf;
	Key sourceKey = cl.createKey (0);
	if (!sourceKey.isValid ())
	{
		throw invalid_argument ("Source given is not a valid keyname");
	}

	Key destKey = cl.createKey (1);
	if (!destKey.isValid ())
	{
		throw invalid_argument ("Destination given is not a valid keyname");
	}
	string newDirName = destKey.getName ();

	kdb.get (conf, sourceKey);
	kdb.get (conf, destKey);
	KeySet tmpConf = conf;
	KeySet oldConf;

	oldConf.append (tmpConf.cut (sourceKey));

	KeySet newConf;

	oldConf.rewind ();
	std::string sourceName = sourceKey.getName ();
	if (cl.verbose) cout << "common name: " << sourceName << endl;
	if (cl.recursive)
	{
		// copy all keys with new name
		Key k;
		while ((k = oldConf.next ()))
		{
			Key rk = rename_key (k, sourceName, newDirName, cl.verbose);
			copySingleKey (cl, rk, tmpConf, newConf);
		}
	}
	else
	{
		// just copy one key
		Key k = oldConf.next ();
		Key rk = rename_key (k, sourceName, newDirName, cl.verbose);
		copySingleKey (cl, rk, tmpConf, newConf);
	}

	newConf.append (tmpConf); // these are unrelated keys
	newConf.append (oldConf); // these are the original keys

	newConf.rewind ();
	kdb.set (newConf, destKey);

	return 0;
}

CpCommand::~CpCommand ()
{
}
