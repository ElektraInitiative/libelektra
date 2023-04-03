/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordstop.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordStopCommand::RecordStopCommand () = default;
RecordStopCommand::~RecordStopCommand () = default;

int RecordStopCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey ("/");

	if (!ckdb::elektraRecordDisableRecording (*kdb, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

	return 0;
}
