#ifndef BENCHMARKS_KSLOOKUP_BENCHMARK_H
#define BENCHMARKS_KSLOOKUP_BENCHMARK_H

// TODO KURT QUEST strange valgrind in gendata ,hsearch and kslookup:
// still reachable: 72,704 bytes in 1 blocks

#include <kdbinternal.h>

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
//~ #include <unistd.h>
#include <sys/time.h>

// never set min = max
#define MIN_KEYSET_SIZE 10
#define STEP_KEYSET_SIZE 10
#define MAX_KEYSET_SIZE 100

#define MIN_BUCKET_STEP 3
#define MAX_BUCKET_STEP 30

#define REPEATS 11 // needs to be odd

/* defines how many versions for a
 * KeySet will be made
 */
#define KEYSET_VERSIONS 1

/* syntax for KeySet dump files:
 * KeySetSize_version.edf
 * and benchmark output files:
 * [hsearch|unorderedmap]_[b|s]_KeySetSize_version.bench
 * or
 * kslookup_[b|s]_version.bench
 * be CAREFUL with buffer size
 */
#define BUFFER_FILENAME 50

#define GENDATA_KEY_VALUE "some data"


unsigned int genRand (void);

//stat helper
int median (int values[], int count);
int max (int values[], int count);

//search helper
int searchNext (int keys_searched_for[], int size);


#endif
