/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordundo.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordUndoCommand::RecordUndoCommand () = default;
RecordUndoCommand::~RecordUndoCommand () = default;

int RecordUndoCommand::execute (const Cmdline & cmdline)
{
	if (cmdline.arguments.size () > 1)
	{
		throw invalid_argument ("at max 1 argument needed");
	}

	Key parentKey ("/");
	if (cmdline.arguments.size () == 1)
	{
		parentKey = cmdline.createKey (0);
	}

	KDB kdb;
	Key errorKey ("/");

	if (!ckdb::elektraRecordUndo (*kdb, *kdb, *parentKey, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	return 0;
}
