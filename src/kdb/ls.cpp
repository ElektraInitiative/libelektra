#include <ls.hpp>

#include <iostream>
#include <kdb>

using namespace kdb;
using namespace std;

LsCommand::LsCommand()
{}

int LsCommand::execute(int argc, char** argv)
{
	if (argc < 3) return 1;

	kdb.get(ks, argv[2]);
	ks.rewind();
	Key k;
	while (k=ks.next())
	{
		cout << "key: " << k.getName() << " " << k.getString() << endl;;
	}

	return 0;
}

LsCommand::~LsCommand()
{}
