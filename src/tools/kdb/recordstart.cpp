/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "coloredkdbio.hpp"
#include "recordstart.hpp"
#include <cmdline.hpp>
#include <kdb.hpp>
#include <kdbrecord.h>
#include <iostream>

using namespace std;
using namespace kdb;

RecordStartCommand::RecordStartCommand () = default;
RecordStartCommand::~RecordStartCommand () = default;

int RecordStartCommand::execute (const Cmdline & cmdline)
{
	if (cmdline.arguments.size() > 1)
	{
		throw invalid_argument ("max 1 argument needed");
	}

	KDB kdb;
	Key parentKey ("/");
	Key errorKey ("/");

	if (cmdline.arguments.size() == 1)
	{
		parentKey = cmdline.createKey (0);
	}

	if (!ckdb::elektraRecordEnableRecording (*kdb, *parentKey, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	return 0;
}
