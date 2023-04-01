/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "coloredkdbio.hpp"
#include "recordstop.hpp"
#include <kdb.hpp>
#include <kdbrecord.h>
#include <cmdline.hpp>
#include <iostream>

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

	return 0;
}
