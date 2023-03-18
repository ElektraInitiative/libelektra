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

#include <kdbtypes.h>

/**
 * Order Preserving Minimal Perfect Hash Map
 *
 * Based on the work of
 *
 * Fabiano C. Botelho and Nivio Ziviani
 * "Near-Optimal Space Perfect Hashing Algorithms"
 *
 * and
 *
 * Zbigniew J. Czech, George Havas, and Bohdan S. Majewski
 * "An Optimal Algorithm for Generating Minimal Perfect Hash Functions"
 * In: Information Processing Letters 43 (1992), pp. 257–264
 *
 * For usage look in [datastructures.md](/doc/dev/datastructures.md)
 *
 * Theoretical limit of elements:
 *
 * The whole OPMPHM is limited to the opmphmHashfunction (...) that returns a uint32_t.
 * The limit of elements is than ((2^32) - 1) * r / c, since (2^32) - 1 is the maximum component size of the
 * r-uniform r-partite hypergraph.
 *
 * To save space the limit of elements is set to (2^32) - 1.
 */

/**
 * The r-uniform r-partite hypergraph
 */
typedef struct
{
	uint32_t order;	     /*!< desired hash map return value */
	uint32_t * nextEdge; /*!< arary with Opmphm->rUniPar indices of the next edge in the lists */
	uint32_t * vertices; /*!< array with Opmphm->rUniPar indices of vertices that the edge connects */
} OpmphmEdge;

typedef struct
{
	uint32_t firstEdge; /*!< first edge in the list of edges of a vertex */
	uint32_t degree;    /*!< length of the list */
} OpmphmVertex;

typedef struct
{
	OpmphmEdge * edges;	   /*!< array of all edges */
	OpmphmVertex * vertices;   /*!< array of all vertices */
	uint32_t * removeSequence; /*!< remove sequence of acyclic r-uniform r-partite hypergraph */
	uint32_t removeIndex;	   /*!< the index used for insertion in removeSequence */
} OpmphmGraph;

/**
 * Opmphm Flags.
 */
typedef enum
{
	OPMPHM_FLAG_MMAP_STRUCT = 1 /*!<
		 Opmphm struct lies inside a mmap region.
		 This flag is set for Opmphm structs inside a mapped region.
		 It prevents erroneous free() calls on these Opmphm structs. */
	,
	OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS = 1 << 2 /*!<
		Opmphm hashFunctionSeeds lies inside a mmap region.
		This flag is set for Opmphm hashFunctionSeeds inside a mapped region.
		It prevents erroneous free() calls on these hashFunctionSeeds. */
	,
	OPMPHM_FLAG_MMAP_GRAPH = 1 << 3 /*!<
		Opmphm graph lies inside a mmap region.
		This flag is set for Opmphm graphs inside a mapped region.
		It prevents erroneous free() calls on these graphs. */
} opmphmflag_t;

/**
 * The opmphm
 */
typedef struct
{
	int32_t * hashFunctionSeeds; /*!< arary with Opmphm->rUniPar seeds for the hash function calls */
	uint8_t rUniPar;	     /*! < number of components in the r-uniform r-partite hypergraph */
	size_t componentSize;	     /*!< the number of vertices in one part of the r-uniform r-partite hypergraph */
	uint32_t * graph;	     /*!< array containing the final OPMPHM */
	size_t size;		     /*!< size of g in bytes */
	opmphmflag_t flags;	     /*!< internal flags */
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
	void ** data;	       /*!< the data */
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
