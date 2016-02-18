/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
		printWarnings (cerr, parentKey);
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
	Key cur;
	std::vector<std::string> mountpoints;
	mountpoints.push_back ("system/elektra");
	mountConf.rewind ();
	while ((cur = mountConf.next ()))
	{
		if (cur.getBaseName () == "mountpoint")
		{
			if (cur.getString ().at (0) == '/')
			{
				mountpoints.push_back (Key ("user" + cur.getString (), KEY_END).getName ());
				mountpoints.push_back (Key ("system" + cur.getString (), KEY_END).getName ());
			}
			else
			{
				mountpoints.push_back (cur.getString ());
			}
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
		mountConf.rewind ();
		while (Key k = mountConf.next ())
		{
			cout << k.getName () << " " << k.getString () << endl;
		}
	}

	if (cl.interactive)
	{
		cout << "Are you sure you want to do that (y/N): ";
		std::string answer;
		cin >> answer;
		if (answer != "y")
			throw CommandAbortException ();
	}

	if (cl.debug)
	{
		cout << "Now writing the mountpoint configuration";
	}
}

class KDBMountException : public KDBException
{
	std::string msg;

public:
	KDBMountException (std::string const & e) : KDBException (Key ()) { msg = e; }

	virtual const char * what () const noexcept override { return msg.c_str (); }
};

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
		throw KDBMountException (std::string (e.what ()) +
					 "\n\n"
					 "IMPORTANT: Make sure you can write to system namespace\n"
					 "           Usually you need to be root for that!");
	}

	printWarnings (cerr, parentKey);
}
