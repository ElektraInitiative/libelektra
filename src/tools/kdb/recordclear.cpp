/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordclear.hpp"
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordClearCommand::RecordClearCommand () = default;
RecordClearCommand::~RecordClearCommand () = default;

int RecordClearCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey ("/");

	ckdb::elektraRecordClearSession (*kdb, *errorKey);
}
