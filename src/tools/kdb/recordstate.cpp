/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordstate.hpp"
#include "coloredkdbio.hpp"
#include "kdbprivate.h"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>

using namespace std;
using namespace kdb;

RecordStateCommand::RecordStateCommand () = default;
RecordStateCommand::~RecordStateCommand () = default;

int RecordStateCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey ("/");

	if (!ckdb::elektraRecordIsActive (*kdb))
	{
		cout << "Recording is not enabled" << endl;
		return 0;
	}

	KeySet r;
	kdb.get (r, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY);
	Key activeKey = r.lookup (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY);

	ckdb::ElektraDiff * cdiff;

	if (!ckdb::elektraRecordGetDiff (*kdb, &cdiff, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 1;
	}

	printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

	ElektraDiff diff (cdiff);

	KeySet addedKeys = diff.getAddedKeys ();
	KeySet modifiedKeys = diff.getModifiedKeys ();
	KeySet removedKeys = diff.getRemovedKeys ();

	cout << "Recording is active for " << activeKey.getString () << endl;
	cout << endl;

	cout << "Added " << addedKeys.size () << " key(s)" << endl;
	cout << "Modified " << modifiedKeys.size () << " key(s)" << endl;
	cout << "Removed " << removedKeys.size () << " key(s)" << endl;
	cout << endl;

	for (const auto & key : addedKeys)
	{
		cout << "Added key " << key.getName () << endl;
	}

	for (const auto & key : modifiedKeys)
	{
		cout << "Modified key " << key.getName () << endl;
	}

	for (const auto & key : removedKeys)
	{
		cout << "Removed key " << key.getName () << endl;
	}

	return 0;
}
