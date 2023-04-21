/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <metaset.hpp>

#include <iostream>
#include <string>

#include <cmdline.hpp>
#include <kdb.hpp>

using namespace std;
using namespace kdb;

MetaSetCommand::MetaSetCommand ()
{
}

int MetaSetCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () < 2 || cl.arguments.size () > 3)
	{
		throw invalid_argument ("Need 2 or 3 arguments");
	}
	string metaname = cl.arguments[1];

	Key k = cl.createKey (0);
	string keyname = k.getName ();

	bool cascadingWrite = keyname[0] == '/';

	Key parentKey = cl.getParentKey (k);

	KeySet conf;
	kdb.get (conf, parentKey);
	k = conf.lookup (k);

	if (!k)
	{
		if (cascadingWrite)
		{
			cerr << "Aborting: A cascading write to a non-existent key is ambiguous." << endl;
			return 12;
		}

		k = Key (keyname, KEY_END);
		// k.setBinary(0, 0); // conceptually maybe better, but would have confusing "binary" metadata
		conf.append (k);
		if (cl.verbose) cout << "Creating key " << keyname << endl;
	}
	if (!k.isValid ())
	{
		cerr << "Could not create key " << keyname << endl;
		return 11;
	}

	if (cl.arguments.size () == 2)
	{
		if (!cl.quiet) cout << "Only two arguments, thus deleting metaname " << metaname << endl;
		k.delMeta (metaname);
	}
	else
	{
		std::string metavalue = cl.arguments[2];
		if (metaname == "atime" || metaname == "mtime" || metaname == "ctime")
		{
			stringstream str (metavalue);
			time_t t;
			str >> t;
			if (!str.good ()) throw "conversion failure";
			k.setMeta<time_t> (metaname, t);
		}
		else
		{
			k.setMeta<string> (metaname, metavalue);
		}
	}

	if (!cl.quiet && cascadingWrite) std::cout << "Using name " << k.getName () << std::endl;

	kdb.set (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	printError (cerr, k, cl.verbose, cl.debug);

	return 0;
}

MetaSetCommand::~MetaSetCommand ()
{
}
