/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <showmeta.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <kdbmacros.h>
#include <kdbproposal.h> // for some options

#include <iostream>

#include <kdbmacros.h>

using namespace std;
using namespace kdb;

ShowMetaCommand::ShowMetaCommand ()
{
}

namespace
{


std::string getCascadingName (std::string name)
{
	if (name[0] == '/') return name;
	if (name.find ('/') == std::string::npos) return "/";
	return name.substr (name.find ('/'));
}
} // namespace


int ShowMetaCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	KeySet conf;

	kdb::Key root = cl.createKey (0);
	kdb::KDB kdb (root);

	std::string n;
	if (cl.all)
	{
		n = root.getName ();
		root.setName ("/");
	}
	return 0;
}

ShowMetaCommand::~ShowMetaCommand ()
{
}
