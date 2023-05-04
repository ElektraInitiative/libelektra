/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordreset.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordResetCommand::RecordResetCommand () = default;
RecordResetCommand::~RecordResetCommand () = default;

int RecordResetCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey;

	if (!ckdb::elektraRecordResetSession (*kdb, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

	return 0;
}
