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

	std::string command = argv[1];
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
	KeySet oldConf;

	oldConf.append (tmpConf.cut(sourceKey));

	KeySet newConf;

	Key k;
	oldConf.rewind();
	std::string commonName = sourceKey.getName();
	cout << "common name: " << commonName << endl;
	while (k = oldConf.next())
	{
		std::string otherName = k.getName();
		std::string baseName = otherName.substr(commonName.length());
		cout << "key: " << otherName <<
			" will be copied/moved to: " << newDirName + baseName <<  endl;

		Key newKey = k.dup();
		newKey.setName (newDirName + baseName);
		newConf.append(newKey);
	}
	newConf.append(tmpConf); // these are unrelated keys
	if (command == "cp" || command == "cp-r")
	{
		cout << "in preserving (cp) mode" << endl;
		newConf.append(oldConf); // these are the original keys
	} // else simple drop the original conf

	newConf.rewind();
	cout << "Will write out:" << endl;
	while (Key k = newConf.next())
	{
		cout << k.getName() << " " << k.getString() << endl;
	}

	kdb.set(newConf, destKey);

	return 0;
}

CpCommand::~CpCommand()
{}
