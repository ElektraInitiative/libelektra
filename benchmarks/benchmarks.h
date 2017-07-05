/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef BENCHMARKS_H
#define BENCHMARKS_H

#include <kdb.h>
#include <kdbinternal.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <time.h>

#define KEY_ROOT "user/benchmark"

#define KEY_NAME_LENGTH 1000
#define NUM_DIR 200
#define NUM_KEY 200

#define NR 50

#define BUF_SIZ 50

void timeInit (void);
void timePrint (char * msg);

void benchmarkCreate ();
void benchmarkFillup ();

extern int num_dir; // default = NUM_DIR;
extern int num_key; // default = NUM_KEY;
extern KeySet * large;

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS

/**
 * KeySetShape
 * Set minWordLength < maxWordLength and parent > 0.
 * Not all Keys must have a direct parent,
 * with the parent parameter the occurrence of direct parents can be influenced,
 * the probability of having a direct parent is (1/parent).
 * The shapef is used to determine the number of sub Keys.
 * The parameter of the shapef are the following (in order):
 *
 * initSize: is the maximal size of the KeySet
 * size: the remaining space in the KeySet
 * level: the actual level (root is 1)
 * seed: a seed for random actions
 *
 * The shapef should retuen a value >= 0 and <= size.
 *
 */

typedef size_t (*KsShapeFunction) (size_t, size_t, int, uint32_t *);
typedef struct
{
	int minWordLength;      /*!< min Length of a string between two '/' */
	int maxWordLength;      /*!< max Length of a string between two '/' */
	int parent;		/*!< defines the parent behaviour */
	KsShapeFunction shapef; /*!< Function Pointer for the KeySet shape */
} KeySetShape;

KeySet * generateKeySet (size_t size, uint32_t * seed, KeySetShape * shape);

#endif

#endif
