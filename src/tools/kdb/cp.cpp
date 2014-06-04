#include <cp.hpp>

#include <kdb.hpp>
#include <rename.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

CpCommand::CpCommand()
{}

int CpCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 2)
	{
		throw invalid_argument("wrong number of arguments, 2 needed");
	}

	KeySet conf;
	Key sourceKey(cl.arguments[0], KEY_END);
	if (!sourceKey.isValid())
	{
		throw invalid_argument("Source given is not a valid keyname");
	}

	Key destKey(cl.arguments[1], KEY_END);
	if (!destKey.isValid())
	{
		throw invalid_argument("Destination given is not a valid keyname");
	}
	string newDirName = cl.arguments[1];

	kdb.get(conf, sourceKey);
	kdb.get(conf, destKey);
	KeySet tmpConf = conf;
	KeySet oldConf;

	oldConf.append (tmpConf.cut(sourceKey));

	KeySet newConf;

	oldConf.rewind();
	std::string sourceName = sourceKey.getName();
	if (cl.verbose) cout << "common name: " << sourceName << endl;
	if (cl.recursive)
	{
		// copy all keys with new name
		Key k;
		while (k = oldConf.next())
		{
			newConf.append(rename_key(k, sourceName, newDirName, cl.verbose));
		}
	}
	else
	{
		// just copy one key
		Key k = oldConf.next();
		newConf.append(rename_key(k, sourceName, newDirName, cl.verbose));
	}

	newConf.append(tmpConf); // these are unrelated keys
	newConf.append(oldConf); // these are the original keys

	newConf.rewind();
	if (cl.verbose)
	{
		cout << "Will write out:" << endl;
		cout << newConf;
	}

	kdb.set(newConf, destKey);

	return 0;
}

CpCommand::~CpCommand()
{}
