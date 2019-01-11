/**
 * @file
 *
 * @brief Elektra error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_H
#define ELEKTRA_ERROR_H

#include <elektra/errorcodes.h>
#include <kdb.h>
#include <stdbool.h>

#ifdef __cplusplus
#define Key ckdb::Key
extern "C" {
#endif

typedef struct _ElektraError ElektraError;

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

int elektraKDBErrorCode (const ElektraError * error);
const char * elektraKDBErrorDescription (const ElektraError * error);
ElektraErrorSeverity elektraKDBErrorSeverity (const ElektraError * error);
ElektraKDBErrorGroup elektraKDBErrorGroup (const ElektraError * error);
ElektraKDBErrorModule elektraKDBErrorModule (const ElektraError * error);
const char * elektraKDBErrorReason (const ElektraError * error);
int elektraKDBErrorWarningCount (const ElektraError * error);
ElektraError * elektraKDBErrorGetWarning (const ElektraError * error, int index);
Key * elektraKDBErrorKey (const ElektraError * error);

void elektraErrorReset (ElektraError ** error);

#ifdef __cplusplus
}
#undef Key
#endif

#endif // ELEKTRA_ERROR_H
