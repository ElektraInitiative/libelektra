/**
 * @file
 *
 * @brief Elektra error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_H
#define ELEKTRA_ERROR_H

#include "types.h"
#include <elektra/old_kdb.h>
#include <stdbool.h>

#ifdef __cplusplus
#define Key ckdb::Key
extern "C" {
#endif

typedef struct _ElektraError ElektraError;

typedef void (*ElektraErrorHandler) (ElektraError * error);

ElektraError * elektraErrorPureWarning (void);

const char * elektraErrorCode (const ElektraError * error);
const char * elektraErrorDescription (const ElektraError * error);

void elektraErrorReset (ElektraError ** error);

// for code-generation
ElektraError * elektraErrorConversionToString (KDBType sourceType, const char * keyname);
ElektraError * elektraErrorConversionFromString (KDBType targetType, const char * keyname, const char * sourceValue);

#ifdef __cplusplus
}
#undef Key
#endif

#endif // ELEKTRA_ERROR_H
