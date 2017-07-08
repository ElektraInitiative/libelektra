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
#include <kdbassert.h>
#include <kdbinternal.h>
#include <kdbrand.h>

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

/**
 * Arbitrary Key Set Generator
 *
 * KeySetShape
 * Not all Keys must have a direct parent,
 * with the parent parameter the occurrence of direct parents can be influenced,
 * the probability of having a direct parent is (1/parent).
 * Same goes for occurrences of special chars (not [a-zA-z0-9]),
 * see alphabetnumbers and alphabetspecial in benchmarks.c for more information.
 *
 * Restrictions due to elektraRand:
 *
 * maxWordLength - minWordLength <= 16777215
 * parent <= 127
 * special <= 127
 *
 * The shapef is used to determine the number of sub Keys.
 * shapef will be executed on each node in the hierarchal tree of your KeySet.
 * The parameter of the shapef are the following (in order):
 *
 * initSize: is the maximal size of the KeySet
 * size: the remaining space in the KeySet
 * level: the actual level (root is 1)
 * seed: a seed for random actions
 *
 * The shapef should retuen always a value >= 0 and <= size.
 *
 * Example:
 * Call: generateKeySet (100,...
 * KeySet:
 * /test/foo
 * /test/foo/bar
 *
 * the shapef parameters are:
 * initSize= 100
 * size= 98
 * level= 3
 *
 */

typedef size_t (*KsShapeFunction) (size_t, size_t, size_t, int32_t *);
typedef struct
{
	unsigned int minWordLength; /*!< min Length of a string between two '/' */
	unsigned int maxWordLength; /*!< max Length of a string between two '/' */
	unsigned int parent;	/*!< defines the parent behaviour */
	unsigned int special;       /*!< defines the special char behaviour */
	KsShapeFunction shapef;     /*!< Function Pointer for the KeySet shape */
} KeySetShape;

KeySet * generateKeySet (size_t size, int32_t * seed, KeySetShape * shape);

#endif
