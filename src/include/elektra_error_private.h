/**
 * @file
 *
 * @brief Elektra Error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_PRIVATE_H
#define ELEKTRA_ERROR_PRIVATE_H

#include "elektra_error.h"
#include "elektra_types.h"
#include "kdb.h"

struct _ElektraError
{
	ElektraErrorCode code;
	char * description;
	ElektraErrorSeverity severity;
	ElektraKDBError * lowLevelError;
};

struct _ElektraKDBError
{
	int code;
	const char * description;
	ElektraErrorSeverity severity;
	ElektraKDBErrorGroup group;
	ElektraKDBErrorModule module;
	const char * reason;
	int warningCount;
	ElektraKDBError ** warnings;
	Key * errorKey;
};

ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity);
ElektraError * elektraErrorCreateFromKey (Key * key);

#endif // ELEKTRA_ERROR_PRIVATE_H
