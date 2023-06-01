/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef BENCHMARKS_H
#define BENCHMARKS_H

#include <elektra/core/errors.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>
#include <internal/config.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/rand.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <time.h>

#define KEY_ROOT "user:/benchmark"

#define KEY_NAME_LENGTH 1000
#define NUM_DIR 200
#define NUM_KEY 200

#define NR 50

#define BUF_SIZ 50

void timeInit (void);
void timePrint (char * msg);
int timeGetDiffMicroseconds (void);

void benchmarkCreate (void);
void benchmarkFillup (void);

extern int num_dir; // default = NUM_DIR;
extern int num_key; // default = NUM_KEY;
extern KeySet * large;


/**
 * Arbitrary Key Set Generator
 */
extern const char * const alphabetnumbers;
extern const char * const alphabetspecial;

/**
 * The Arbitrary Key Set Generator sees the KeySet as Tree, where the root ("/") and every name is a vertex.
 *
 * The KeySetShape has the following fields:
 *
 *  * minWordLength and maxWordLength:
 *     The range of Key name length.
 *     Restrictions:
 *      * maxWordLength - minWordLength <= 16777215 (due to elektraRand (...))
 *      * maxWordLength >= minWordLength
 *      * 0 < maxWordLength
 *      * 0 < minWordLength
 *  * parent:
 *     Influences the random behaviour if a vertex is a Key in the resulting KeySet.
 *     If 0 no parents at all, if > 0 the probability is 1/parent.
 *     Restriction:
 *      * 0 <= parent <= 127 (due to elektraRand (...))
 *  * special:
 *     Influences the random vertex name generation.
 *     If 0 no special characters at all, if > 0 the probability is 1/special.
 *     See alphabetnumbers and alphabetspecial in benchmarks.c for the characters.
 *     Restriction:
 *      * 0 <= special <= 127 (due to elektraRand (...))
 *  * shapef:
 *     This function pointer is invoked for every vertex (except the root) and determines the child vertices.
 *     The parameters are the following (in order):
 *      * const size_t initSize: is the maximal size of the resulting KeySet
 *      * size_t size: the remaining number of Keys
 *      * size_t level: the actual level (root is 0)
 *      * int32_t * seed: a seed for random actions, not scrambled
 *      * KsShapeFunctionReturn * ret: see below
 *      * void * data: see below
 *     KsShapeFunctionReturn:
 *      Is the return value of the shapef and has two fields subKeys and label.
 *      There are two modes:
 *       * Create fresh subKeys:
 *          Set subKeys >= 0 and label = 0, to create a branch you do not want to reuse.
 *          Set subKeys >= 0 and label > 0, to create a branch you want to link later, using the label number.
 *       * Use an already existing branch:
 *          Set subKeys < 0 and label > 0, the subKeys will be taken from the labelled branch.
 *     Data:
 *      The shapef function has also a memory, the data pointer points to it.
 *  * shapeInit:
 *     Initializes the data passed to shapef. On error just return NULL.
 *  * shapeDefaultDel:
 *     Deletes the data used from shapef.
 * Example:
 *                    o // root
 *                   / \
 *                  3.  1.
 *                     / \
 *                    o   2.
 *
 * 1.:
 *  level = 1, we set ret->subKeys = 2 and ret->label = 1
 *  The vertex 1. has 2 subKeys and a label is set, to reference later.
 * 2.:
 *  level = 2, we set ret->subKeys = 0 and ret->label = 0
 *  The vertex at 2. has no subKeys so there will be a Key with "/(name of 1.)/(name of 2.)" in the resulting KeySet, no label set.
 * 3.:
 *  level = 1, we set ret->subKeys = -1 and ret->label = 1
 *  The children of vertex 1. will be copied to this vertex.
 *
 * Possible Resulting KeySet (name can differ):
 * /asd/bar
 * /asd/foo
 * /test/bar
 * /test/foo
 *
 * Another example is the Default KeySetShape in benchmarks.c
 */
typedef struct
{
	ssize_t subKeys;
	size_t label;
} KsShapeFunctionReturn;

typedef void (*KsShapeFunction) (const size_t, size_t, size_t, int32_t *, KsShapeFunctionReturn *, void *);

typedef void * (*KsShapeInit) (void);
typedef void (*KsShapeDel) (void *);


typedef struct
{
	unsigned int minWordLength; /*!< min Length of a string between two '/' */
	unsigned int maxWordLength; /*!< max Length of a string between two '/' */
	unsigned int parent;	    /*!< defines the parent behaviour */
	unsigned int special;	    /*!< defines the special char behaviour */
	KsShapeFunction shapef;	    /*!< function pointer for the KeySet shape */
	KsShapeInit shapeInit;	    /*!< function pointer for the KeySet shape data initialization */
	KsShapeDel shapeDel;	    /*!< function pointer for the KeySet shape data deletion */
} KeySetShape;

KeySet * generateKeySet (const size_t size, int32_t * seed, KeySetShape * shape);

void printExit (const char * msg);

#endif
