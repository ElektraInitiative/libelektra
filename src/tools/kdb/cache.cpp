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
	}
	else if (cmd == "disable")
	{
		conf.lookup (enabled, KDB_O_POP);
	}
	else if (cmd == "clear")
	{
		throw "not implemented";
	}
	else
	{
		throw invalid_argument ("1 argument required");
	}

	kdb.set (conf, parentKey);
	//cerr << "Mountpoint " << name << " does not exist" << endl;
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	return 0;
}

CacheCommand::~CacheCommand ()
{
}
