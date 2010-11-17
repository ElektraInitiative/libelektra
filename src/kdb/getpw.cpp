#include <getpw.hpp>

#include <kdb>

#include <ctype.h>

#include <iostream>
#include <algorithm>

using namespace std;
using namespace kdb;

GetPwCommand::GetPwCommand()
{}

int GetPwCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: getpw <name>" << endl;
		return 1;
	}

	KeySet conf;
	Key x("user/kwallet", KEY_END);
	kdb.get(conf, x);

	std::string search (argv[2]);
	std::transform (search.begin(), search.end(), search.begin(),
			::toupper);

	Key cur;
	conf.rewind();
	while (cur = conf.next())
	{
		std::string name = cur.getName();
		std::transform (name.begin(), name.end(), name.begin(),
				::toupper);
		if (name.find(search) != string::npos)
		{
			cout << "Key Name: " << cur.getName() << endl;
			cout << "Password: ";
			cout.write (static_cast<const
					char*>(ckdb::keyValue(*cur)),
					cur.getValueSize());
			cout << endl;
		}
	}


	return 0;
}

GetPwCommand::~GetPwCommand()
{}
