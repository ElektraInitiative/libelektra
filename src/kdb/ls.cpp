#include <ls.hpp>

#include <iostream>
#include <sstream>
#include <iomanip>

#include <kdb>

using namespace kdb;
using namespace std;

LsCommand::LsCommand()
{}

int LsCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: ls <name>" << endl;
		return 1;
	}

	Key root (argv[2], KEY_END);
	if (!root)
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	kdb.get(ks, root);
	ks.rewind();
	Key k;
	while (k=ks.next())
	{
		cout << "key: " << k.getName() << " " << k.getString() << endl;;
	}

	try{
		int nr = root.getMeta<int>("warnings");
		cerr << nr+1 << " Warnings were issued" << endl;

		for (int i=0; i<=nr; i++)
		{
			ostringstream name;
			name << "warnings/#" << setfill('0') << setw(2) << i;
			std::cerr << name.str() << ": " << root.getMeta<std::string>(name.str()) << std::endl;
			std::cerr << "number: " << root.getMeta<std::string>(name.str() + "/number") << std::endl;
			std::cerr << "description: " << root.getMeta<std::string>(name.str() + "/description") << std::endl;
			std::cerr << "ingroup: " << root.getMeta<std::string>(name.str() + "/ingroup") << std::endl;
			std::cerr << "module: " << root.getMeta<std::string>(name.str() + "/module") << std::endl;
			std::cerr << "at: " << root.getMeta<std::string>(name.str() + "/file") << ":"
				<< root.getMeta<std::string>("error/line") << std::endl;
			std::cerr << "reason: " << root.getMeta<std::string>(name.str() + "/reason") << std::endl;
		}

	} catch (KeyMetaException const& e)
	{
		// no warnings were issued
	}

	return 0;
}

LsCommand::~LsCommand()
{}
