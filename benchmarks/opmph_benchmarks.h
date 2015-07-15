#ifndef OPMPH_BENCHMARKS_H
#define OPMPH_BENCHMARKS_H

#include <kdb.h>
#include <kdbinternal.h>

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

#define MIN_KEYSET_SIZE 10
#define MAX_KEYSET_SIZE 10
#define STEP_KEYSET_SIZE 10

#define REPEATS 11 // needs to be odd

KeySet * generateKeySet (size_t size);

unsigned int genRand (void);

#endif
