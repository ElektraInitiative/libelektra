#ifndef BENCHMARKS_KSLOOKUP_BENCHMARK_H
#define BENCHMARKS_KSLOOKUP_BENCHMARK_H

#include <kdbinternal.h>

#include <sys/time.h>

#define MIN_KEYSET_SIZE 10
#define STEP_KEYSET_SIZE 10
#define MAX_KEYSET_SIZE 100

/* defines how many versions for a
 * KeySet will be made
 */
#define KEYSETS_PER_SIZE 10

/* syntax for KeySet dump files:
 * size_version.edf
 * be CAREFUL with buffer size
 */
#define BUFFER_FILENAME_SIZE 20


unsigned int genRand (void);


#endif
