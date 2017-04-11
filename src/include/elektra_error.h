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

void elektraErrorDel (ElektraError * error);

#endif //ELEKTRA_ERROR_H