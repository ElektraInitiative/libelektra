#include <cp.hpp>

#include <kdb>

#include <iostream>

using namespace std;
using namespace kdb;

CpCommand::CpCommand()
{}

int CpCommand::execute(int argc, char** argv)
{
	if (argc != 4)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: cp <source> <dest>" << endl;
		return 1;
	}

	KeySet conf;
	Key sourceKey(argv[2], KEY_END);
	if (!sourceKey)
	{
		cerr << "Source given is not a valid keyname" << endl;
		return 1;
	}

	Key destKey(argv[3], KEY_END);
	if (!destKey)
	{
		cerr << "Destination given is not a valid keyname" << endl;
		return 1;
	}
	string newDirName = argv[3];

	kdb.get(conf, sourceKey);
	kdb.get(conf, destKey);
	KeySet tmpConf = conf;
	KeySet oldConf = tmpConf.cut(sourceKey);

	KeySet newConf;

	Key k;
	oldConf.rewind();
	while (k = oldConf.next())
	{
		std::string commonName = sourceKey.getName();
		std::string otherName = k.getName();
		std::string baseName = otherName.substr(commonName.length());
		cout << "common name: " << commonName << " otherName: " << otherName <<
			" base name: " << baseName << " new name: " << newDirName + baseName <<  endl;

		Key newKey = k.dup();
		newKey.setName (newDirName + baseName);
		newConf.append(newKey);
	}

	newConf.rewind();
	while (Key k = newConf.next())
	{
		cout << k.getName() << " " << k.getString() << endl;
	}

	kdb.set(newConf, destKey);

	return 0;
}

CpCommand::~CpCommand()
{}
