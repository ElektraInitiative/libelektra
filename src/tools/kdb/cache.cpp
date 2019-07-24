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
#include <modules.hpp>

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
	string cmd = cl.arguments[0];

	Key enabled ("system/elektra/cache/enabled", KEY_END);
	if (cmd == "enable")
	{
		KDB kdb;
		kdb.get (conf, parentKey);
		printWarnings (cerr, parentKey, cl.verbose, cl.debug);

		conf.append (enabled);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "disable")
	{
		KDB kdb;
		kdb.get (conf, parentKey);
		printWarnings (cerr, parentKey, cl.verbose, cl.debug);

		conf.lookup (enabled, KDB_O_POP);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "clear")
	{
		Modules modules;
		PluginPtr plugin = modules.load ("cache", cl.getPluginsConfig ());

		KeySet ks;
		Key errorKey ("system/elektra/cache/clear", KEY_END);
		errorKey.setMeta ("cacheClear", "YES");

		plugin->get (ks, errorKey);

		printWarnings (cerr, errorKey, cl.verbose, cl.debug);
		printError (cerr, errorKey, cl.verbose, cl.debug);
	}
	else
	{
		throw invalid_argument ("1 argument required");
	}

	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	printError (cerr, parentKey, cl.verbose, cl.debug);
	return 0;
}

CacheCommand::~CacheCommand ()
{
}
