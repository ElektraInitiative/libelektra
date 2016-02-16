/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <ls.hpp>

#include <iostream>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

LsCommand::LsCommand() : kdb(root)
{}

int LsCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1)
	{
		throw invalid_argument("1 argument required");
	}

	printWarnings(cerr, root);

	root = cl.createKey(0);

	kdb.get(ks, root);

	if (cl.verbose) cout << "size of all keys in mountpoint: " << ks.size() << endl;

	KeySet part (ks.cut(root));

	if (cl.verbose) cout << "size of requested keys: " << part.size() << endl;
	cout.setf(std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf(std::ios_base::skipws);
	}

	cout << part;

	printWarnings(cerr, root);

	return 0;
}

LsCommand::~LsCommand()
{}
