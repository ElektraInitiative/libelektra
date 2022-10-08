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
#include <kdblogger.h>

#include <iostream>

using namespace std;
using namespace kdb;

FileCommand::FileCommand ()
{
}

int FileCommand::execute (Cmdline const & cl)
{
	// FIXME (kodebach): fix, document in kdbGet
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	Key x = cl.createKey (0);

	if (!x.isValid ())
	{
		throw invalid_argument (cl.arguments[0] + " is not a valid keyname");
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
