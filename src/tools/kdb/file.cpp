/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <file.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

FileCommand::FileCommand()
{}

int FileCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	std::string name = cl.arguments[0];
	if (name[0] == '/')
	{
		name = cl.ns + name;
		std::cerr << "Using name " << name << std::endl;
	}
	Key x(name, KEY_END);
	if (!x.isValid())
	{
		throw invalid_argument(cl.arguments[0] + " is not an valid keyname");
	}

	kdb.get(conf, x);
	cout << x.getString();

	if (!cl.noNewline)
	{
		cout << endl;
	}

	return 0;
}

FileCommand::~FileCommand()
{}
