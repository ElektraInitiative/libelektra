/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <mountbase.hpp>

#include <backend.hpp>
#include <backends.hpp>
#include <cmdline.hpp>

#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>


using namespace std;
using namespace kdb;
using namespace kdb::tools;

/**
 * @brief Read in configuration and print warnings
 *
 * @post will update mountConf
 */
void MountBaseCommand::readMountConf (Cmdline const & cl)
{
	Key parentKey (mountpointsPath, KEY_END);

	kdb.get (mountConf, parentKey);

	if (!cl.null && cl.first && cl.second && cl.third)
	{
		printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	}
}

void MountBaseCommand::outputMissingRecommends (std::vector<std::string> missingRecommends)
{
	if (!missingRecommends.empty ())
	{
		std::cout << "Missing recommended plugins: ";
		for (auto const & p : missingRecommends)
		{
			std::cout << p << " ";
		}
		std::cout << std::endl;
	}
}


/**
 * @brief set mp (interactive or by commandline)
 *
 * @see getName()
 */
void MountBaseCommand::getMountpoint (Cmdline const & cl)
{
	std::vector<std::string> mountpoints;
	mountpoints.push_back ("system:/elektra");

	for (Key cur : mountConf)
	{
		if (cur.getBaseName () == "mountpoint")
		{
			mountpoints.push_back (cur.getString ());
		};
	}

	if (cl.interactive)
	{
		cout << "Already used are: ";
		std::copy (mountpoints.begin (), mountpoints.end (), ostream_iterator<std::string> (cout, " "));
		cout << endl;
		cout << "Please start with / for a cascading backend" << endl;
		cout << "Enter the mountpoint: ";
		cin >> mp;
	}
	else
	{
		mp = cl.createKey (1).getName ();
	}
}

void MountBaseCommand::askForConfirmation (Cmdline const & cl)
{
	if (cl.interactive)
	{
		cout << endl;
		cout << "Ready to mount with following configuration:" << endl;
		cout << "Mountpoint: " << mp << endl;
		cout << "Path:       " << path << endl;
	}

	if (cl.debug)
	{
		cout << "The configuration which will be set is:" << endl;
		for (Key k : mountConf)
		{
			cout << k.getName () << " " << k.getString () << endl;
		}
	}

	if (cl.interactive)
	{
		cout << "Are you sure you want to do that (y/N): ";
		std::string answer;
		cin >> answer;
		if (answer != "y") throw CommandAbortException ();
	}

	if (cl.debug)
	{
		cout << "Now writing the mountpoint configuration";
	}
}

/**
 * @brief Really write out config
 */
void MountBaseCommand::doIt ()
{
	Key parentKey (mountpointsPath, KEY_END);

	try
	{
		kdb.set (mountConf, parentKey);
	}
	catch (KDBException const & e)
	{
		throw KDBMountException (
			std::string (e.what ()) +
			"\n\n"
			"IMPORTANT: Sorry, I am unable to write your requested mountpoint to system:/elektra/mountpoints.\n"
			"           You can get the problematic file name by reading the elektra system file (kdb file "
			"system:/elektra/mountpoints).\n" +
			getErrorColor (ANSI_COLOR::BOLD) + getErrorColor (ANSI_COLOR::YELLOW) +
			"           Usually you need to be root for this operation (try `sudo !!`)." + getErrorColor (ANSI_COLOR::RESET));
	}

	printWarnings (cerr, parentKey, true, true);
}
