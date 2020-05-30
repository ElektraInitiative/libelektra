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
	Key parentKey ("system:/elektra/cache", KEY_END);

	KDB kdb;
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	string cmd = cl.arguments[0];
	Key isEnabled ("system:/elektra/cache/enabled", KEY_END);
	if (cmd == "enable")
	{
		// always use the cache
		isEnabled.setString ("1");
		conf.append (isEnabled);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "disable")
	{
		// never use the cache
		isEnabled.setString ("0");
		conf.append (isEnabled);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "default")
	{
		// reset to default settings, use cache if available
		conf.lookup (isEnabled, KDB_O_POP);
		kdb.set (conf, parentKey);
	}
	else if (cmd == "clear")
	{
		Modules modules;
		PluginPtr plugin = modules.load ("cache", cl.getPluginsConfig ());

		KeySet ks;
		parentKey.setMeta ("cache/clear", "1");
		plugin->get (ks, parentKey);
	}
	else
	{
		throw invalid_argument ("not a valid subcommand");
	}

	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	printError (cerr, parentKey, cl.verbose, cl.debug);
	return 0;
}

CacheCommand::~CacheCommand ()
{
}
