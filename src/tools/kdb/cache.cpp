/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cache.hpp>

#include <backends.hpp>
#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

CacheCommand::CacheCommand ()
{
}

int CacheCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("1 argument required");

	KeySet conf;
	Key parentKey ("system/elektra/cache", KEY_END);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	string cmd = cl.arguments[0];

	Key enabled ("system/elektra/cache/enabled", KEY_END);
	if (cmd == "enable")
	{
		conf.append (enabled);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "disable")
	{
		conf.lookup (enabled, KDB_O_POP);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "clear")
	{
		Key wasEnabled = conf.lookup (enabled, KDB_O_POP);

		// enable cache so it can clear cache files
		if (wasEnabled == nullptr)
		{
			conf.append (enabled);
			kdb.set (conf, parentKey);
		}

		KeySet tmp;
		KDB tmpKDB;
		Key errorKey ("system/elektra/cache/clear", KEY_END);
		errorKey.setMeta ("cacheClear", "YES");
		tmpKDB.get (tmp, errorKey); // global plugin will be called and will clear the cache

		// disable cache again if it was previously disabled
		if (wasEnabled == nullptr)
		{
			conf.lookup (enabled, KDB_O_POP);
			kdb.set (conf, parentKey);
		}
	}
	else
	{
		throw invalid_argument ("1 argument required");
	}

	//cerr << "Mountpoint " << name << " does not exist" << endl;
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	return 0;
}

CacheCommand::~CacheCommand ()
{
}
