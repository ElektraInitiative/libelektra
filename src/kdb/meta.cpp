#include <meta.hpp>

#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

MetaCommand::MetaCommand()
{}

int MetaCommand::execute(int argc, char** argv)
{
	string prog = argv[0];
	string command = argv[1];
	if (command == "meta-get" && argc < 4)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <meta-name>" << endl;
		return 1;
	} else if (command == "meta-set" && argc < 5)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <meta-name> <meta-value>" << endl;
		return 1;
	}
	string keyname = argv[2];
	string metaname = argv[3];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	Key k = conf.lookup(keyname);

	if (command == "meta-get")
	{
		if (!k) cout << "Key not found" << endl;
		else cout << k.getMeta<string>(metaname) << endl;
	} else if (command == "meta-set")
	{
		if (!k) k = Key(keyname, KEY_END);
		if (!k.isValid())
		{
			cout << "Could not create key" << endl;
			return 1;
		}

		std::string metavalue = argv[4];
		if (metaname == "atime" || metaname == "mtime" || metaname == "ctime")
		{
			stringstream str (metavalue);
			time_t t;
			str >> t;
			if (!str.good()) throw "conversion failure";
			k.setMeta<time_t> (metaname, t);
		} else {
			k.setMeta<string> (metaname, metavalue);
		}

		Key parentKey(keyname, KEY_END);
		kdb.set(conf,parentKey);
	}

	return 0;
}

MetaCommand::~MetaCommand()
{}
