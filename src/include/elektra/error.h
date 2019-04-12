/**
 * @file
 *
 * @brief Elektra error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_H
#define ELEKTRA_ERROR_H

#include <kdb.h>
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

#ifdef __cplusplus
}
#undef Key
#endif

#endif // ELEKTRA_ERROR_H
