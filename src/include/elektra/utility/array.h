/**
 * @file
 *
 * @brief Helpers for array indices
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_ARRAY_H
#define ELEKTRA_UTILITY_ARRAY_H

#include <elektra/type/types.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraWriteArrayNumber (char * newName, kdb_long_long_t newIndex);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_ARRAY_H
