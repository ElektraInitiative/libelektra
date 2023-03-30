/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordstart.hpp"
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordStartCommand::RecordStartCommand () = default;
RecordStartCommand::~RecordStartCommand () = default;

int RecordStartCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key parentKey ("/");
	Key errorKey ("/");

	ckdb::elektraRecordEnableRecording (*kdb, *parentKey, *errorKey);
}
