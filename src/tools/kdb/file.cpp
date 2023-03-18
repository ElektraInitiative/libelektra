/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <file.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <internal/utility/logger.h>

#include <iostream>

using namespace std;
using namespace kdb;

FileCommand::FileCommand ()
{
}

int FileCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	Key x = cl.createKey (0);

	if (!x.isValid ())
	{
		throw invalid_argument (cl.arguments[0] + " is not a valid keyname");
	}

	// Not all namespaces are supported, because we don't know if the key actually exists
	// If it does not exist, the key name alone must uniquely determine the storage location
	// That's only the case for spec:/, system:/, user:/ and dir:/ keys
	if (x.getNamespace () == ElektraNamespace::CASCADING)
	{
		throw invalid_argument ("Cannot retrieve the file for a cascading key. Use a concrete namespace.");
	}

	if (x.getNamespace () != ElektraNamespace::SPEC && x.getNamespace () != ElektraNamespace::SYSTEM &&
	    x.getNamespace () != ElektraNamespace::USER && x.getNamespace () != ElektraNamespace::DIR)
	{
		throw invalid_argument ("Can only retrieve file for a persistable namespace: spec:/, system:/, user:/ or dir:/");
	}

	try
	{
		kdb.get (conf, x);
	}
	catch (KDBException const & exception)
	{
		// The command should return the filename even if the config file contains syntax errors
		ELEKTRA_LOG_WARNING ("Get returned with an exception: %s", exception.what ());
	}
	cout << x.getString ();

	if (!cl.noNewline)
	{
		cout << endl;
	}

	return 0;
}

FileCommand::~FileCommand ()
{
}
