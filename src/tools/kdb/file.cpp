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
	Key x = cl.createKey(0);
	if (x.getName()[0] == '/')
	{
		x.setName (cl.ns + x.getName());
		std::cerr << "Using name " << x.getName() << std::endl;
	}
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
