/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <validation.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

ValidationCommand::ValidationCommand()
{}

int ValidationCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();
	if (argc != 3 && argc != 4)
	{
		throw invalid_argument("need 3 or 4 arguments");
	}
	string keyname = cl.arguments[0];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	Key k = conf.lookup(keyname);

	if (!k)
	{
		k = Key(keyname, KEY_END);
		conf.append (k);
	}

	if (!k.isValid())
	{
		throw invalid_argument("keyname not valid");
	}

	string value = cl.arguments[1];
	string validationregex = cl.arguments[2];
	string validationmessage;
	if (argc == 4) validationmessage = cl.arguments[3];
	else validationmessage = "Regular expression " + validationregex + " does not match the supplied value";

	k.setString (value);
	k.setMeta<string> ("validation/regex", validationregex);
	k.setMeta<string> ("validation/message", validationmessage);

	kdb.set(conf,parentKey);

	return 0;
}

ValidationCommand::~ValidationCommand()
{}
