/**
 * @file
 *
 * @brief Defines for the Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */
#ifndef OPMPHM_H
#define OPMPHM_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * The Order Preserving Minimal Perfect Hash Map (OPMPHM) first maps each key name to a k-tuple, k is stored in OPMPHMTUPLE.
 * Each element of the k-tuples is in the range 0 to width. The width (abbreviated with w) determines how many keys can fit in the OPMPHM.
 * w^OPMPHMTUPLE is the maximum number of keys that fit in. w is set dynamically to the minimum value that:
 *
 * w^OPMPHMTUPLE > opmphmRatio * n
 *
 * opmphmRatio should never be less than 1.
 *
 */
#define OPMPHMTUPLE 5
extern double opmphmRatio;

/**
 * Typedefs used to reduce the memory footprint
 */

typedef size_t opmphmTranssition_t;

/**
 * Saves a k-tuple in h[] and tells the OPMPHM the return value of each element, only needed during build.
 */
typedef struct
{
	size_t h[OPMPHMTUPLE]; /*!< the k-tuple filled by the OPMPHM with the hash function values */
	union {
		size_t p;	      /*!< external usage*/
		size_t t[OPMPHMTUPLE]; /*!< internal usage */
	} index;		       /*!< desired hash map return value */
} OpmphmOrder;

/**
 * Only needed for Initialisation.
 */
typedef const char * (*opmphmGetString) (void *);
typedef struct
{
	opmphmGetString getString; /*!< Function pointer used to extract the key name from the data. */
	void ** data;		   /*!< The data */
	int32_t initSeed;	  /*!< seed used to determine opmphmHashFunctionSeeds */
	size_t minOrder;	   /*!< min hash map return value */
	size_t maxOrder;	   /*!< max hash map return value */
} OpmphmInit;


/**
 * Opmphm represents the final OPMPHM.
 * This struct is needed for the lookup, during the lookup the key name gets hashed OPMPHMTUPLE times
 * with different seeds from opmphmHashFunctionSeeds.
 */
typedef struct
{
	opmphmTranssition_t * transsitions[OPMPHMTUPLE]; /*!< stores the opmphm automata */
	size_t size[OPMPHMTUPLE];			 /*!< stores the opmphm automata size in bytes */
	uint32_t opmphmHashFunctionSeeds[OPMPHMTUPLE];   /*!< the seed for the hash function calls */
	size_t outputBase;				 /*!< the base of the output */
} Opmphm;

/**
 * Only needed internal for Build.
 */
typedef struct
{
	size_t vertex;
	size_t input;
	bool isBuffer;
} OpmphmStack;

/**
 * Basic functions
 */
Opmphm * opmphmNew ();
void opmphmDel (Opmphm * opmphm);
void opmphmClear (Opmphm * opmphm);
bool opmphmIsEmpty (Opmphm * opmphm);

/**
 * Build functions
 */
OpmphmOrder * opmphmNewOrder (size_t n, bool opmphm);
OpmphmOrder ** opmphmInit (Opmphm * opmphm, OpmphmInit * init, OpmphmOrder * order, size_t n);
int opmphmMapping (Opmphm * opmphm, OpmphmInit * init, OpmphmOrder * order, OpmphmOrder ** sortOrder, size_t n);
int opmphmBuild (Opmphm * opmphm, OpmphmOrder ** sortOrder, size_t n);

/**
 * Lookup function
 */
size_t opmphmLookup (Opmphm * opmphm, const void * name, size_t n);

/**
 * Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 */

#define OPMPHM_HASHFUNCTION_ROT(x, k) (((x) << (k)) | ((x) >> (32 - (k))))

#define OPMPHM_HASHFUNCTION_FINAL(a, b, c)                                                                                                 \
	{                                                                                                                                  \
		c ^= b;                                                                                                                    \
		c -= OPMPHM_HASHFUNCTION_ROT (b, 14);                                                                                      \
		a ^= c;                                                                                                                    \
		a -= OPMPHM_HASHFUNCTION_ROT (c, 11);                                                                                      \
		b ^= a;                                                                                                                    \
		b -= OPMPHM_HASHFUNCTION_ROT (a, 25);                                                                                      \
		c ^= b;                                                                                                                    \
		c -= OPMPHM_HASHFUNCTION_ROT (b, 16);                                                                                      \
		a ^= c;                                                                                                                    \
		a -= OPMPHM_HASHFUNCTION_ROT (c, 4);                                                                                       \
		b ^= a;                                                                                                                    \
		b -= OPMPHM_HASHFUNCTION_ROT (a, 14);                                                                                      \
		c ^= b;                                                                                                                    \
		c -= OPMPHM_HASHFUNCTION_ROT (b, 24);                                                                                      \
	}

#define OPMPHM_HASHFUNCTION_MIX(a, b, c)                                                                                                   \
	{                                                                                                                                  \
		a -= c;                                                                                                                    \
		a ^= OPMPHM_HASHFUNCTION_ROT (c, 4);                                                                                       \
		c += b;                                                                                                                    \
		b -= a;                                                                                                                    \
		b ^= OPMPHM_HASHFUNCTION_ROT (a, 6);                                                                                       \
		a += c;                                                                                                                    \
		c -= b;                                                                                                                    \
		c ^= OPMPHM_HASHFUNCTION_ROT (b, 8);                                                                                       \
		b += a;                                                                                                                    \
		a -= c;                                                                                                                    \
		a ^= OPMPHM_HASHFUNCTION_ROT (c, 16);                                                                                      \
		c += b;                                                                                                                    \
		b -= a;                                                                                                                    \
		b ^= OPMPHM_HASHFUNCTION_ROT (a, 19);                                                                                      \
		a += c;                                                                                                                    \
		c -= b;                                                                                                                    \
		c ^= OPMPHM_HASHFUNCTION_ROT (b, 4);                                                                                       \
		b += a;                                                                                                                    \
	}
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval);

#endif
