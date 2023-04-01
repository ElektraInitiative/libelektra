/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordclear.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <kdb.hpp>
#include <kdbrecord.h>
#include <iostream>

using namespace std;
using namespace kdb;

RecordClearCommand::RecordClearCommand () = default;
RecordClearCommand::~RecordClearCommand () = default;

int RecordClearCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey ("/");

	if (!ckdb::elektraRecordClearSession (*kdb, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	return 0;
}
