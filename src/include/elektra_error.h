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

typedef struct _ElektraError ElektraError;

typedef enum {
    ELEKTRA_ERROR_SEVERITY_FATAL = 0,
    ELEKTRA_ERROR_SEVERITY_ERROR,
    ELEKTRA_ERROR_SEVERITY_WARNING
} ElektraErrorSeverity;

typedef const char * ElektraErrorGroup;
typedef const char * ElektraErrorModule;

ElektraErrorCode elektraErrorCode (ElektraError * error);
const char * elektraErrorDescription (ElektraError * error);
ElektraErrorSeverity elektraErrorSeverity (ElektraError * error);
ElektraErrorGroup elektraErrorGroup (ElektraError * error);
ElektraErrorModule elektraErrorModule (ElektraError * error);

void elektraErrorReset (ElektraError ** error);

#endif //ELEKTRA_ERROR_H
