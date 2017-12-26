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

#ifdef KDBRAND_BENCHMARK
extern int32_t elektraRandBenchmarkInitSeed;
#endif

int32_t elektraRandGetInitSeed (void);

#ifdef __cplusplus
}
}
#endif


#endif
