/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <plugininfo.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <modules.hpp>
#include <plugin.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;


PluginInfoCommand::PluginInfoCommand ()
{
}

int PluginInfoCommand::execute (Cmdline const & cl)
{
	std::string subkey;
	if (cl.arguments.size () < 1 || cl.arguments.size () > 2)
	{
		throw invalid_argument ("Need 1 or 2 argument(s)");
	}

	if (cl.arguments.size () == 2)
	{
		subkey = cl.arguments[1];
	}
	std::string name = cl.arguments[0];

	KeySet conf;
	Key parentKey (std::string ("system:/elektra/modules/") + name, KEY_END);

	if (!cl.load)
	{
		KDB kdb;
		kdb.get (conf, parentKey);
	}

	if (!conf.lookup (parentKey))
	{
		if (!cl.load)
		{
			cerr << "Module does not seem to be loaded." << endl;
			cerr << "Now in fallback code. Will directly load config from plugin." << endl;
		}

		Modules modules;
		KeySet ks = cl.getPluginsConfig ();
		PluginPtr plugin;
		if (ks.size () == 0)
		{
			plugin = modules.load (name);
		}
		else
		{
			plugin = modules.load (name, ks);
		}

		// fix name for virtual plugins
		if (name != plugin->name ())
		{
			std::cerr << "Will use name " << plugin->name () << " for virtual plugin named " << name << std::endl;
			name = plugin->name ();
		}
		conf.append (plugin->getInfo ());
	}

	Key root (std::string ("system:/elektra/modules/") + name + "/exports", KEY_END);

	if (!subkey.empty ())
	{
		root.setName (std::string ("system:/elektra/modules/") + name + "/infos/" + subkey);
		Key k = conf.lookup (root);
		if (k)
		{
			cout << k.getString () << std::endl;
			return 0;
		}
		else
		{
			cerr << "clause not found" << std::endl;
			return 11;
		}
	}

	root.setName (std::string ("system:/elektra/modules/") + name + "/exports");


	ssize_t it = conf.search (root) + 1;
	if (it > 0)
	{
		cout << "Exported symbols: ";

		for (; it < conf.size (); ++it)
		{
			if (!conf.at (it).isBelow (root)) break;
			cout << conf.at (it).getBaseName () << " ";
		}
		cout << endl;
	}
	else
		cout << "no exported symbols found" << endl;

	root.setName (std::string ("system:/elektra/modules/") + name + "/infos");
	it = conf.search (root) + 1;

	if (it > 0)
	{
		for (; it < conf.size (); ++it)
		{
			if (!conf.at (it).isBelow (root)) break;
			cout << getStdColor (ANSI_COLOR::BOLD) << conf.at (it).getBaseName () << ": " << getStdColor (ANSI_COLOR::RESET)
			     << conf.at (it).getString () << endl;
		}
	}
	else
		cout << "no information found" << endl;

	return 0;
}

PluginInfoCommand::~PluginInfoCommand ()
{
}
