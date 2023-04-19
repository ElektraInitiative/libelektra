/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include "recordrm.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordRemoveKeyCommand::RecordRemoveKeyCommand () = default;
RecordRemoveKeyCommand::~RecordRemoveKeyCommand () = default;

int RecordRemoveKeyCommand::execute (const Cmdline & cmdline)
{
       if (cmdline.arguments.size () != 1)
       {
	       throw invalid_argument ("needs 1 argument");
       }

       KDB kdb;
       Key parentKey = cmdline.createKey (0);
       Key errorKey ("/");

       if (!ckdb::elektraRecordRemoveKey(*kdb, *parentKey, *errorKey))
       {
	       printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
	       return 1;
       }

       printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

       return 0;
}
