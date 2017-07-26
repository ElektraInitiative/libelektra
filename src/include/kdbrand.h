/**
 * @file
 *
 * @brief Defines for Rand.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */
#ifndef KDBRAND_H
#define KDBRAND_H

#include <stdint.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/*Random*/
#define ELEKTRARANDMAX 2147483647
void elektraRand (int32_t * seed);

#ifdef __cplusplus
}
}
#endif


#endif
