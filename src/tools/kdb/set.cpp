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
	if (argc != 1 && argc != 2)
	{
		throw invalid_argument ("1 or 2 arguments needed");
	}

	bool nullValue;
	std::string value;

	if (argc == 2)
	{
		nullValue = false;
		value = cl.arguments[1];
	}
	else
	{
		nullValue = true;
	}

	KeySet conf;
	Key k = cl.createKey (0);
	std::string name = k.getName ();

	// do not resume on any get errors
	// otherwise the user might break
	// the config
	kdb.get (conf, k);

	if (name[0] == '/')
	{
		// fix name for lookup
		name = cl.ns + name;
		if (!cl.quiet) std::cout << "Using name " << name << std::endl;

		// fix k for kdb.set later
		k.setName (name);
	}

	Key key = conf.lookup (name);

	std::ostringstream toprint;
	if (!key)
	{
		toprint << "Create a new key " << name;
		key = Key (name, KEY_END);
		if (!nullValue)
		{
			toprint << " with string \"" << value << '"' << endl;
			key.setString (value);
		}
		else
		{
			toprint << " with null value" << endl;
			key.setBinary (nullptr, 0);
		}
		if (!key.isValid ())
		{
			cerr << "no valid name supplied" << endl;
			return 1;
		}
		conf.append (key);
	}
	else
	{
		if (!nullValue)
		{
			toprint << "Set string to \"" << value << '"' << endl;
			key.setString (value);
		}
		else
		{
			toprint << "Set null value" << endl;
			key.setBinary (nullptr, 0);
		}
	}
	kdb.set (conf, k);
	printWarnings (cerr, k, cl.verbose, cl.debug);
	printError (cerr, k, cl.verbose, cl.debug);

	if (!cl.quiet) cout << toprint.str ();

	return 0;
}

SetCommand::~SetCommand ()
{
}
