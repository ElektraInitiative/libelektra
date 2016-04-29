/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <validation.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

ValidationCommand::ValidationCommand ()
{
}

int ValidationCommand::execute (Cmdline const & cl)
{
	size_t argc = cl.arguments.size ();
	if (argc != 3 && argc != 4)
	{
		throw invalid_argument ("need 3 or 4 arguments");
	}

	KeySet conf;
	Key parentKey = cl.createKey (0);
	string keyname = parentKey.getName ();
	kdb.get (conf, parentKey);
	Key k = conf.lookup (keyname);

	if (!k)
	{
		k = Key (keyname, KEY_END);
		conf.append (k);
	}

	if (!k.isValid ())
	{
		throw invalid_argument ("keyname not valid");
	}

	string value = cl.arguments[1];
	string validationregex = cl.arguments[2];
	string validationmessage;
	if (argc == 4)
		validationmessage = cl.arguments[3];
	else
		validationmessage = "Regular expression " + validationregex + " does not match the supplied value";

	k.setString (value);
	k.setMeta<string> ("check/validation", validationregex);
	k.setMeta<string> ("check/validation/match", "LINE");
	k.setMeta<string> ("check/validation/message", validationmessage);

	kdb.set (conf, parentKey);

	return 0;
}

ValidationCommand::~ValidationCommand ()
{
}
