/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <check.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <modules.hpp>
#include <plugin.hpp>

#include <iostream>


using namespace std;
using namespace kdb;
using namespace kdb::tools;

CheckCommand::CheckCommand ()
{
}

int printProblems (Key const & k, std::string const & action, int off)
{
	bool wo = k.getMeta<const kdb::Key> ("warnings");
	bool eo = k.getMeta<const kdb::Key> ("error");
	if (wo || eo) std::cerr << "\n======\n" << action << " of kdb yield following problems:" << std::endl;
	printWarnings (cerr, k, true, true);
	printError (cerr, k, true, true);
	return (wo + eo * 2) << off;
}

int doKDBcheck (bool force)
{
	Key x;
	try
	{
		int ret = 0;
		KDB kdb (x);
		ret += printProblems (x, "opening", 0);

		KeySet ks;
		Key a ("/", KEY_END);
		try
		{
			kdb.get (ks, a);
		}
		catch (...)
		{
		}
		ret += printProblems (a, "getting", 2);

		if (force)
		{
			Key b ("/", KEY_END);
			try
			{
				kdb.set (ks, b);
			}
			catch (...)
			{
			}
			ret += printProblems (b, "setting", 4);
		}

		Key y;
		kdb.close (y);
		ret += printProblems (y, "closing", 6);
		return ret;
	}
	catch (KDBException const & e)
	{
		std::cerr << "a fatal problem occurred, could not open KDB. This should not happen" << std::endl;
		return printProblems (x, "opening", 0);
	}
}

int CheckCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () == 0)
	{
		return doKDBcheck (cl.force);
	}

	std::string name = cl.arguments[0];

	Modules modules;
	if (cl.verbose) cout << "will try check the plugin " << name << endl;

	vector<string> warnings;
	try
	{
		KeySet ks = cl.getPluginsConfig ();
		ks.append (Key ("system/module", KEY_END));
		PluginPtr plugin = modules.load (name, ks);
		plugin->check (warnings);
	}
	catch (NoPlugin const & p)
	{
		cerr << p.what () << endl;
		return 1;
	}
	catch (PluginCheckException const & p)
	{
		cerr << "Plugin did not pass all checks:" << endl;
		cerr << "See description below:" << endl;
		cerr << p.what () << endl;
		return 2;
	}

	if (warnings.size () > 0)
	{
		cerr << "There are " << warnings.size () << " warnings for this plugin" << endl;
		cerr << "For high quality plugins there should be no warning" << endl;

		for (size_t i = 0; i < warnings.size (); ++i)
		{
			cerr << "Warning #" << i << ": " << warnings[i] << endl;
		}
		return 3;
	}

	return 0;
}

CheckCommand::~CheckCommand ()
{
}
