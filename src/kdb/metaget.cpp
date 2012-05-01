#include <metaget.hpp>

#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

MetaGetCommand::MetaGetCommand()
{}

int MetaGetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 2)
	{
		throw invalid_argument ("Need 2 arguments");
	}
	string keyname = cl.arguments[0];
	string metaname = cl.arguments[1];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	Key k = conf.lookup(keyname);

	if (!k) cout << "Key not found" << endl;
	else cout << k.getMeta<string>(metaname) << endl;

	return 0;
}

MetaGetCommand::~MetaGetCommand()
{}
