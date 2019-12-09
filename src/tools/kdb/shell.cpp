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
#include <kdbmerge.h>

#include <iostream>
#include <sstream>

using namespace std;
using namespace kdb;

ShellCommand::ShellCommand ()
: supportedCommands (
	  "kdbGet <name> .. get conf into current keyset\n"
	  "kdbSet <name> .. set conf from current keyset\n"
	  "kdbMerge <name> .. merge keys from below current key in database into current keyset\n"
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
		else if (command == "kdbMerge")
		{
			if (currentKey.isValid ())
			{
				string theirRootString;
				is >> theirRootString;
				Key theirRootKey (theirRootString, KEY_END);
				KeySet their;
				kdb.get (their, theirRootKey);
				ckdb::KeySet * result;
				if ((result = ckdb::elektraMerge (current.getKeySet (), currentKey.getKey (), their.getKeySet (),
								  theirRootKey.getKey (), current.getKeySet (), currentKey.getKey (),
								  currentKey.getKey (), ckdb::MERGE_STRATEGY_ABORT,
								  currentKey.getKey ())) != NULL)
				{
					int retVal = current.append (result);
					if (retVal < 0)
					{
						cout << "ERROR: Could not append merge result to current key set! (Got " << retVal << ")"
						     << endl;
					}
					else
					{
						cout << "Merge successful! Current key set is now of size " << retVal << "." << endl;
					}
				}
				else
				{
					cout << "ERROR: Merge API returned NULL!" << endl;
				}
			}
			else
			{
				cout << "ERROR: Current key must be specified!" << endl;
			}
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
			current.rewind ();
			while (current.next ())
			{
				Key const & c = current.current ();
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
