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
void MountBaseCommand::readMountConf()
{
	Key parentKey(Backends::mountpointsPath, KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.get(mountConf, parentKey);
	kdb.close (parentKey);

	printWarnings (cerr, parentKey);
}


/**
 * @brief Check for rootkey in  mountConf and add one if missing
 *
 * @param cl.verbose print text when it is missing
 */
void MountBaseCommand::fixRootKey(Cmdline const& cl)
{
	Key rootKey (Backends::mountpointsPath, KEY_END);

	Key cur;
	cur = mountConf.lookup(rootKey);
	if (!cur)
	{
		if (cl.verbose)
		{
			cout << "Did not find the root key, will add it" << endl;
		}
		mountConf.append ( *Key(Backends::mountpointsPath,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END));
		mountConf.rewind();
	}
}



/**
 * @brief Set variable name (either interactive or by parameter)
 */
void MountBaseCommand::getName(Cmdline const& cl)
{
	std::vector <std::string> names;
	Backends::BackendInfoVector info = Backends::getBackendInfo(mountConf);
	names.push_back("default");
	for (Backends::BackendInfoVector::const_iterator it=info.begin();
			it!=info.end(); ++it)
	{
		names.push_back(it->name);
	}

	if (cl.interactive)
	{
		cout << "Already used are: ";
		std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
		cout << endl;
		std::cout << "Backend name: ";
		cin >> name;
	}
	else
	{
		name = cl.arguments[1];
		if (name == "/")
		{
			name = "root";
		}
		else
		{
			std::replace(name.begin(), name.end(), '/', '_');
		}
	}

	if (std::find(names.begin(), names.end(), name) != names.end()) throw NameAlreadyInUseException();
}

/**
 * @brief set mp (interactive or by commandline)
 *
 * @pre name must be set before
 * @see getName()
 */
void MountBaseCommand::getMountpoint(Cmdline const& cl)
{
	Key cur;
	std::vector <std::string> mountpoints;
	mountpoints.push_back("system/elektra");
	mountConf.rewind();
	while ((cur = mountConf.next()))
	{
		if (cur.getBaseName() == "mountpoint")
		{
			if (cur.getString().at(0) == '/')
			{
				mountpoints.push_back(Key ("user" + cur.getString(), KEY_END).getName());
				mountpoints.push_back(Key ("system" + cur.getString(), KEY_END).getName());
			} else {
				mountpoints.push_back(cur.getString());
			}
		};
	}

	mp = "/";
	if (name != "root")
	{
		if (cl.interactive)
		{
			cout << "Already used are: ";
			std::copy (mountpoints.begin(), mountpoints.end(), ostream_iterator<std::string>(cout, " "));
			cout << endl;
			cout << "Please start with / for a cascading backend" << endl;
			cout << "Enter the mountpoint: ";
			cin >> mp;
		}
		else
		{
			mp = cl.arguments[1];
		}
	}

	if (mp.at(0) == '/')
	{
		Key skmp ("system" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), skmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
		Key ukmp ("user" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), ukmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	} else {
		Key kmp (mp, KEY_END);
		if (!kmp.isValid()) throw MountpointNotValid();
		if (std::find(mountpoints.begin(), mountpoints.end(), kmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	}
}

void MountBaseCommand::askForConfirmation(Cmdline const& cl)
{
	if (cl.interactive)
	{
		cout << endl;
		cout << "Ready to mount with following configuration:" << endl;
		cout << "Name:       " << name << endl;
		cout << "Mountpoint: " << mp << endl;
		cout << "Path:       " << path << endl;
	}

	if (cl.debug)
	{
		cout << "The configuration which will be set is:" << endl;
		mountConf.rewind();
		while (Key k = mountConf.next())
		{
			cout << k.getName() << " " << k.getString() << endl;
		}
	}

	if (cl.interactive)
	{
		cout << "Are you sure you want to do that (y/N): ";
		std::string answer;
		cin >> answer;
		if (answer != "y") throw CommandAbortException();
	}

	if (cl.debug)
	{
		cout << "Now writing the mountpoint configuration";
	}
}

/**
 * @brief Really write out config
 */
void MountBaseCommand::doIt()
{
	Key parentKey(Backends::mountpointsPath, KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.set(mountConf, parentKey);
	kdb.close (parentKey);

	printWarnings(cerr, parentKey);
}

