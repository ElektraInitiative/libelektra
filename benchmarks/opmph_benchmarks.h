#ifndef OPMPH_BENCHMARKS_H
#define OPMPH_BENCHMARKS_H

#ifndef __cplusplus

#include <kdb.h>
#include <kdbinternal.h>

#endif

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#include <sys/time.h>

#define ALPHABET "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
#define MIN_DEPTH 3
#define MAX_DEPTH 10
#define MIN_WORDLENGTH 3
#define MAX_WORDLENGTH 15

#define MIN_KEYSET_SIZE 1000
#define MAX_KEYSET_SIZE 1000
#define STEP_KEYSET_SIZE 10

#define BUCKET_MIN_STEP_COUNT 3
#define BUCKET_MAX_STEP_COUNT 30

#define REPEATS 11 // needs to be odd

#ifdef __cplusplus

ckdb::KeySet * generateKeySet (size_t size);

#else

KeySet * generateKeySet (size_t size);

#endif

unsigned int genRand (void);

#endif
