#include <validation.hpp>

#include <kdb>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

ValidationCommand::ValidationCommand()
{}

int ValidationCommand::execute(int argc, char** argv)
{
	string prog = argv[0];
	string command = argv[1];
	if (argc != 6)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <value> <validation-regex> <validation-message>" << endl;
		return 1;
	}
	string keyname = argv[2];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	Key k = conf.lookup(keyname);

	if (!k) k = Key(keyname, KEY_END);
	if (!k.isValid())
	{
		cout << "Could not create key" << endl;
		return 1;
	}

	string value = argv[3];
	string validationregex = argv[4];
	string validationmessage = argv[5];

	k.setString (value);
	k.setMeta<string> ("validation/regex", validationregex);
	k.setMeta<string> ("validation/message", validationmessage);

	kdb.set(conf,parentKey);

	return 0;
}

ValidationCommand::~ValidationCommand()
{}
