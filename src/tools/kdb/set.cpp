/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <set.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <kdbio.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

SetCommand::SetCommand ()
{
}

int SetCommand::execute (Cmdline const & cl)
{
	int argc = cl.arguments.size ();
	if (argc != 2)
	{
		throw invalid_argument ("2 arguments needed");
	}

	std::string value = cl.arguments[1];

	KeySet conf;
	Key k = cl.createKey (0);
	std::string name = k.getName ();

	// do not resume on any get errors
	// otherwise the user might break
	// the config
	kdb.get (conf, k);

	bool cascadingWrite = name[0] == '/';

	Key key = conf.lookup (name);

	std::ostringstream toprint;
	
	if (!key && cascadingWrite)
	{
		cerr << "Aborting: A cascading write to a non-existent key is ambiguous." << endl;
		return 1;
	}
	if (!key)
	{
		toprint << "Create a new key " << name;
		key = Key (name, KEY_END);
		toprint << " with string \"" << value << '"' << endl;
		key.setString (value);

		if (!key.isValid ())
		{
			cerr << "no valid name supplied" << endl;
			return 1;
		}
		conf.append (key);
	}
	else
	{
		toprint << "Set string to \"" << value << '"' << endl;
		key.setString (value);
	}
	kdb.set (conf, k);
	printWarnings (cerr, k, cl.verbose, cl.debug);
	printError (cerr, k, cl.verbose, cl.debug);

	if (cascadingWrite) toprint << "Using name " << key.getName () << std::endl;
	if (!cl.quiet) cout << toprint.str ();

	return 0;
}

SetCommand::~SetCommand ()
{
}
