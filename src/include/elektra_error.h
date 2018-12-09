/**
 * @file
 *
 * @brief Elektra error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_H
#define ELEKTRA_ERROR_H

#include "elektra_error_codes.h"
#include "kdb.h"

typedef struct _ElektraError ElektraError;
typedef struct _ElektraKDBError ElektraKDBError;

typedef enum
{
	/**
	 * Use only, if the error will be raised with elektraFatalError().
	 */
	ELEKTRA_ERROR_SEVERITY_FATAL = 0,
	ELEKTRA_ERROR_SEVERITY_ERROR,
	ELEKTRA_ERROR_SEVERITY_WARNING
} ElektraErrorSeverity;

typedef const char * ElektraKDBErrorGroup;
typedef const char * ElektraKDBErrorModule;

typedef void (*ElektraErrorHandler) (ElektraError * error);

ElektraErrorCode elektraErrorCode (const ElektraError * error);
const char * elektraErrorDescription (const ElektraError * error);
ElektraErrorSeverity elektraErrorSeverity (const ElektraError * error);
ElektraKDBError * elektraErrorLowLevelError (const ElektraError * error);

int elektraKDBErrorCode (const ElektraKDBError * error);
const char * elektraKDBErrorDescription (const ElektraKDBError * error);
ElektraErrorSeverity elektraKDBErrorSeverity (const ElektraKDBError * error);
ElektraKDBErrorGroup elektraKDBErrorGroup (const ElektraKDBError * error);
ElektraKDBErrorModule elektraKDBErrorModule (const ElektraKDBError * error);
const char * elektraKDBErrorReason (const ElektraKDBError * error);
int elektraKDBErrorWarningCount (const ElektraKDBError * error);
const ElektraKDBError ** elektraKDBErrorWarnings (const ElektraKDBError * error);
Key * elektraKDBErrorKey (const ElektraKDBError * error);

void elektraErrorReset (ElektraError ** error);

#endif // ELEKTRA_ERROR_H
