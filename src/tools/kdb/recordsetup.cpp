/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordsetup.hpp"
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordSetupCommand::RecordSetupCommand () = default;
RecordSetupCommand::~RecordSetupCommand () = default;

int RecordSetupCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey ("/");

	ckdb::elektraRecordSetup ();
}
