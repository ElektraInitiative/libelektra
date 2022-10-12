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
	else
	{
		Key key = tmpConf.lookup (rk);
		if (key != nullptr)
		{
			if ((key.isString () && key.getString () != rk.getString ()) ||
			    (key.isBinary () && key.getBinary () != rk.getBinary ()))
			{
				throw CommandAbortException (std::string ("Copy will not be done, because " + rk.getName () +
									  " already exists and has a different value"
									  ", use -f to force copy"),
							     11);
			}
		}
	}
	newConf.append (rk);
}
} // namespace

int CpCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 2)
	{
		throw invalid_argument ("wrong number of arguments, 2 needed");
	}

	KeySet conf;
	Key sourceKey = cl.createKey (0, false);

	Key destKey = cl.createKey (1, false);

	string newDirName = destKey.getName ();

	kdb.get (conf, sourceKey);
	kdb.get (conf, destKey);
	KeySet tmpConf = conf;
	KeySet oldConf;

	std::string sourceName = sourceKey.getName ();
	oldConf.append (tmpConf.cut (sourceKey));

	if (!oldConf.size ())
	{
		std::cerr << "No key to copy found below '" << sourceName << "'" << std::endl;
		return 11;
	}

	KeySet newConf;

	if (cl.verbose) cout << "common name: " << sourceName << endl;
	if (cl.recursive)
	{
		// copy all keys with new name
		for (Key k : oldConf)
		{
			Key rk = rename_key (k, sourceName, newDirName, cl.verbose);
			copySingleKey (cl, rk, tmpConf, newConf);
		}
	}
	else
	{
		// just copy one key
		Key k = oldConf.at (0);
		if (k != sourceKey)
		{
			cerr << "First key found " << k.getName () << " does not exactly match given key " << sourceKey.getName ()
			     << ", aborting (use -r to move hierarchy)\n";
			return 11;
		}
		Key rk = rename_key (k, sourceName, newDirName, cl.verbose);
		copySingleKey (cl, rk, tmpConf, newConf);
	}

	newConf.append (tmpConf); // these are unrelated keys
	newConf.append (oldConf); // these are the original keys

	kdb.set (newConf, destKey);

	return 0;
}

CpCommand::~CpCommand ()
{
}
