/**
 * @file
 *
 * @brief Source for stdioproc plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "stdioproc.h"

#include <kdbhelper.h>


int elektraStdioprocOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// TODO: parent/child
	//   1. read config
	//   2. create pipes
	//   3. fork and redirect stdin/stdout
	//   child:
	//    4. exec
	//   parent:
	//    4. send handshake, wait for ack
	//     if child implements open:
	//      5. send open
	//      6. update errorKey
	//      7. return result

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraStdioprocClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// TODO: parent only
	// if child implements close:
	//  1. send close
	//  2. update errorKey
	//  3. save result
	// 4. send termination, ?wait for child exit?
	// 5. return result

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraStdioprocGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/stdioproc"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/stdioproc", KEY_VALUE, "stdioproc plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports", KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/open", KEY_FUNC, elektraStdioprocOpen, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/close", KEY_FUNC, elektraStdioprocClose, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/get", KEY_FUNC, elektraStdioprocGet, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/set", KEY_FUNC, elektraStdioprocSet, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/commit", KEY_FUNC, elektraStdioprocCommit, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/error", KEY_FUNC, elektraStdioprocError, KEY_END),
			keyNew ("system:/elektra/modules/stdioproc/exports/checkconf", KEY_FUNC, elektraStdioprocCheckConf, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/stdioproc/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	// TODO: parent only
	// if key matches modules key (with name from child):
	//  1. set contract and return success
	// else:
	//  2. send get
	//  3. update parentKey
	//  4. update returned
	//  5. return result

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraStdioprocSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// TODO: parent only
	// if child implements set
	//  1. send set
	//  2. update parentKey
	//  3. update returned
	//  4. return result
	// else:
	//  return error

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraStdioprocError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// TODO: parent only
	// if child implements error
	//  1. send error
	//  2. update parentKey
	//  3. update returned
	//  4. return result
	// else:
	//  return error

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraStdioprocCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// TODO: parent only
	// if child implements commit
	//  1. send commit
	//  2. update parentKey
	//  3. update returned
	//  4. return result
	// else:
	//  return error

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraStdioprocCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// TODO: parent only
	// if child implements checkConf
	//  1. send checkConf
	//  2. update errorKey
	//  3. update returned
	//  4. return result
	// else:
	//  return error

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("stdioproc",
		ELEKTRA_PLUGIN_OPEN,	&elektraStdioprocOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraStdioprocClose,
		ELEKTRA_PLUGIN_GET,	&elektraStdioprocGet,
		ELEKTRA_PLUGIN_SET,	&elektraStdioprocSet,
		ELEKTRA_PLUGIN_COMMIT,  &elektraStdioprocCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraStdioprocError,
		ELEKTRA_PLUGIN_END);
}
