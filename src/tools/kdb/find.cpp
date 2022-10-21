/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <find.hpp>

#include <iostream>
#include <regex>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

FindCommand::FindCommand ()
{
}

int FindCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	Key root ("/", KEY_END);
	KDB kdb (root);
	KeySet ks;

	printWarnings (cerr, root, cl.verbose, cl.debug);

	kdb.get (ks, root);

	if (cl.verbose) cout << "size of all keys: " << ks.size () << endl;

	KeySet part;
	std::smatch match;

	try
	{
		std::regex reg (cl.arguments[0]);

		for (const auto & it : ks)
		{
			std::string name = it.getName ();
			if (std::regex_search (name, match, reg))
			{
				part.append (it);
			}
		}
	}
	catch (const regex_error & error)
	{
		cerr << "Regex error in “" << cl.arguments[0] << "”: " << error.what () << endl;
	}

	if (cl.verbose) cout << "size of found keys: " << part.size () << endl;
	cout.setf (std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (std::ios_base::skipws);
	}

	std::cout << part;

	printWarnings (cerr, root, cl.verbose, cl.debug);

	return 0;
}

FindCommand::~FindCommand ()
{
}
