/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordstop.hpp"
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

	ckdb::elektraRecordDisableRecording (*kdb, *errorKey);
}

