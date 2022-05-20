/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <shell.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>
#include <sstream>

using namespace std;
using namespace kdb;

ShellCommand::ShellCommand ()
: supportedCommands (
	  "kdbGet <name> .. get conf into current keyset\n"
	  "kdbSet <name> .. set conf from current keyset\n"
	  "keyClear .. clears the current key\n"
	  "keySetName <name> .. set name of current key (without bookmarks!)\n"
	  "keySetMeta <name> <string> .. set meta of current key\n"
	  "keySetString <string> .. set string of current key\n"
	  "ksAppendKey .. append current key to current keyset\n"
	  "ksCut <name> .. cut current keyset\n"
	  "ksOutput .. outputs all keys of current keyset\n")
{
}

int ShellCommand::execute (Cmdline const &)
{
	KeySet current;
	Key currentKey;

	string commandline;
	string prompt = "> ";

	cout << prompt;
	while (getline (cin, commandline))
	{
		istringstream is (commandline);
		string command;

		is >> command;
		if (command == "kdbGet")
		{
			string parent;
			is >> parent;
			Key parentKey (parent, KEY_END);
			cout << "return value: " << kdb.get (current, parentKey) << endl;
		}
		else if (command == "kdbSet")
		{
			string parent;
			is >> parent;
			Key parentKey (parent, KEY_END);
			cout << "return value: " << kdb.set (current, parentKey) << endl;
		}
		else if (command == "keyClear")
		{
			currentKey.clear ();
		}
		else if (command == "keySetName")
		{
			string name;
			is >> name;
			currentKey.setName (name);
		}
		else if (command == "keySetMeta")
		{
			string name;
			is >> name;
			string value;
			is >> value;
			std::string tmp;
			getline (is, tmp);
			value += tmp;
			currentKey.setMeta (name, value);
			cout << "Set meta " << name << " to " << value << endl;
		}
		else if (command == "keySetString")
		{
			string value;
			is >> value;
			std::string tmp;
			getline (is, tmp);
			value += tmp;
			currentKey.setString (value);
		}
		else if (command == "ksAppendKey")
		{
			current.append (currentKey.dup ());
		}
		else if (command == "ksCut")
		{
			string parent;
			is >> parent;
			Key parentKey (parent, KEY_END);

			current.cut (parentKey);
		}
		else if (command == "ksOutput")
		{
			for (ssize_t it = 0; it < current.size (); ++it)
			{
				Key const & c = current.at (it);
				cout << c.getName ();
				if (c.isString ())
				{
					cout << " string: " << c.getString () << endl;
				}
				else
				{
					cout << " binary: " << c.getBinary () << " (length: " << c.getBinarySize () << ")" << endl;
				}
			}
		}
		else
		{
			cout << "unknown command!\n"
				"supported are:\n"
			     << supportedCommands << endl;
		}

		cout << prompt;
	}

	return 0;
}

ShellCommand::~ShellCommand ()
{
}
