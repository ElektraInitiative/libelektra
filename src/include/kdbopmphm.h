/**
 * @file
 *
 * @brief Defines for the Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */
#ifndef OPMPHM_H
#define OPMPHM_H

#include <stdint.h>
#include <stdlib.h>

/**
 * For usage look in [datastructures.md](/doc/dev/datastructures.md)
 *
 */

/**
 * The r-uniform r-partite hypergraph
 */
typedef struct
{
	size_t order;	/*!< desired hash map return value */
	size_t * nextEdge;   /*!< arary with Opmphm->rUniPar indices of the next edge in the lists */
	uint32_t * vertices; /*!< array with Opmphm->rUniPar indices of vertices that the edge connects */
} OpmphmEdge;

typedef struct
{
	size_t firstEdge; /*!< first edge in the list of edges of a vertex */
	size_t degree;    /*!< length of the list */
} OpmphmVertex;

typedef struct
{
	OpmphmEdge * edges;      /*!< array of all edges */
	OpmphmVertex * vertices; /*!< array of all vertices */
	size_t * removeSequence; /*!< remove sequence of acyclic r-uniform r-partite hypergraph */
	size_t removeIndex;      /*!< the index used for insertion in removeSequence */
} OpmphmGraph;

/**
 * The opmphm
 */
typedef struct
{
	int32_t * hashFunctionSeeds; /*!< arary with Opmphm->rUniPar seeds for the hash function calls */
	uint8_t rUniPar;	     /*! < number of components in the r-uniform r-partite hypergraph */
	size_t componentSize;	/*!< the number of vertices in one part of the r-uniform r-partite hypergraph */
	size_t * graph;		     /*!< array containing the final OPMPHM */
	size_t size;		     /*!< size of g in bytes */
} Opmphm;

/**
 * Functions providing `r` and `c`
 */
double opmphmMinC (uint8_t r);
uint8_t opmphmOptR (size_t n);
double opmphmOptC (size_t n);

/**
 * Graph functions
 */
OpmphmGraph * opmphmGraphNew (Opmphm * opmphm, uint8_t r, size_t n, double c);
void opmphmGraphClear (const Opmphm * opmphm, OpmphmGraph * graph);
void opmphmGraphDel (OpmphmGraph * graph);

/**
 * Only needed for Initialisation.
 */
typedef const char * (*opmphmGetName) (void *);
typedef struct
{
	opmphmGetName getName; /*!< function pointer used to extract the key name from the data. */
	void ** data;	  /*!< the data */
	int32_t initSeed;      /*!< seed used to determine opmphmHashFunctionSeeds */
} OpmphmInit;

/**
 * Build functions
 */
int opmphmMapping (Opmphm * opmphm, OpmphmGraph * graph, OpmphmInit * init, size_t n);
int opmphmAssignment (Opmphm * opmphm, OpmphmGraph * graph, size_t n, int defaultOrder);

/**
 * Lookup functions
 */
size_t opmphmLookup (Opmphm * opmphm, size_t n, const void * name);

/**
 * Basic functions
 */
Opmphm * opmphmNew (void);
void opmphmDel (Opmphm * opmphm);
int opmphmIsBuild (const Opmphm * opmphm);
int opmphmCopy (Opmphm * dest, const Opmphm * source);
void opmphmClear (Opmphm * opmphm);

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
