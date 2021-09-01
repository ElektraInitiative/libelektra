/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <libgen.h>
#include <mergehelper.hpp>
#include <nameparthelper.hpp>
#include <string>

using namespace kdb;
using namespace std;

string keyNamespace (Key const & key)
{
	string keyname = key.getName ();
	return key.isCascading () ? "" : keyname.substr (0, keyname.find ("/"));
}

string keyBasename (Key const & key)
{
	return key.getBaseName ();
}

string keyDirname (Key const & key)
{
	string keynameWithoutNamespace = removeNamespace (key).getName ();

	// convert to c string
	char * keynameWithoutNamespace_c_str = new char[keynameWithoutNamespace.length () + 1];
	strcpy (keynameWithoutNamespace_c_str, keynameWithoutNamespace.c_str ());

	// apply dirname, convert back
	string dirname_ (dirname (keynameWithoutNamespace_c_str));

	// release c string memory
	delete[] keynameWithoutNamespace_c_str;

	return dirname_;
}

int executeNamepartcommand (Cmdline const & cl, string (*namepart_getter) (Key const &))
{
	int argc = cl.arguments.size ();
	if (argc != 1)
	{
		throw invalid_argument ("1 argument needed");
	}

	Key key = cl.createKey (0);
	cout << (*namepart_getter) (key);

	if (!cl.noNewline)
	{
		cout << endl;
	}

	return 0;
}
