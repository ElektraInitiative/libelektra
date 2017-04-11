/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_H
#define ELEKTRA_ERROR_H

typedef struct _ElektraError ElektraError;

typedef enum {
    // TODO: Specify errors.
    ELEKTRA_ERROR_GENERAL_ERROR = 0
} ElektraErrorCode;

ElektraErrorCode elektraErrorCode (ElektraError * error);
const char * elektraErrorMessage (ElektraError * error);
void elektraErrorDel (ElektraError * error);

#endif //ELEKTRA_ERROR_H