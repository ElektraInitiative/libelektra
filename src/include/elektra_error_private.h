/**
 * @file
 *
 * @brief Elektra Error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_PRIVATE_H
#define ELEKTRA_ERROR_PRIVATE_H

#include "kdb.h"

struct _ElektraError
{
	ElektraErrorCode code;
	const char * description;
	ElektraErrorSeverity severity;
	ElektraErrorGroup group;
	ElektraErrorModule module;
};

ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity, ElektraErrorGroup group,
				   ElektraErrorModule module);
ElektraError * elektraErrorCreateFromKey (Key * key);

#endif // ELEKTRA_ERROR_PRIVATE_H
