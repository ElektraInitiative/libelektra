/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <globalumount.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <algorithm>
#include <iostream>

using namespace std;
using namespace kdb;

GlobalUmountCommand::GlobalUmountCommand ()
{
}

int GlobalUmountCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("1 argument required");

	KeySet conf;
	// they are all mounted in that array
	Key parentKey ("system:/elektra/globalplugins/postcommit/user/plugins/", ELEKTRA_KEY_END);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	const string name = cl.arguments[0];
	auto it =
		find_if (conf.begin (), conf.end (), [&] (Key key) { return key.isDirectBelow (parentKey) && key.get<string> () == name; });

	if (it == conf.end ())
	{
		cerr << "Global Plugin " << name << " does not exist" << endl;
		return 1;
	}

	conf.cut (*it);
	kdb.set (conf, parentKey);

	return 0;
}

GlobalUmountCommand::~GlobalUmountCommand ()
{
}
