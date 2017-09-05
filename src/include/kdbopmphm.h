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
 * The Order Preserving Minimal Perfect Hash Map (opmphm) maps each element to an edge in a r-partite hypergraph.
 * The r-partite hypergraph consist of `OPMPHMR_PARTITE` components, each component has `Opmphm->p` vertices.
 * An edge connects `OPMPHMR_PARTITE` vertices, each one in separate components of the r-partite hypergraph.
 * The number of vertices in one component of the r-partite hypergraph (`Opmphm->p`) is calculated during
 * `opmphmGraphNew ()` the following way:
 * ```
 * Opmphm->p = (c * n / OPMPHMR_PARTITE) + 1;
 * ```
 * The finals size of the opmphm (`Opmphm->g`) is: `Opmphm->p * OPMPHMR_PARTITE`
 */

#define OPMPHMR_PARTITE 3

/**
 * The r-partite hypergraph
 */
typedef struct
{
	uint32_t h[OPMPHMR_PARTITE];      /*!< hash function values and index of vertices which the edge connects*/
	size_t order;			  /*!< desired hash map return value */
	size_t nextEdge[OPMPHMR_PARTITE]; /*!< index of the next edge in the lists */
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
	size_t * removeOrder;    /*!< remove sequence of acyclic r-partite hypergraph */
	size_t removeIndex;      /*!< the index used for insertion in removeOrder  */
} OpmphmGraph;

/**
 * The opmphm
 */
typedef struct
{
	size_t * g;					  /*!< array containing the final opmphm */
	size_t size;					  /*!< size of g in bytes */
	int32_t opmphmHashFunctionSeeds[OPMPHMR_PARTITE]; /*!< the seed for the hash function calls */
	size_t p;					  /*!< the number of vertices in one part of the r-partite hypergraph */
} Opmphm;

/**
 * Graph functions
 */
OpmphmGraph * opmphmGraphNew (Opmphm * opmphm, size_t n, double c);
void opmphmGraphDel (OpmphmGraph * graph);

/**
 * Only needed for Initialisation.
 */
typedef const char * (*opmphmGetString) (void *);
typedef struct
{
	opmphmGetString getString; /*!< function pointer used to extract the key name from the data. */
	void ** data;		   /*!< the data */
	int32_t initSeed;	  /*!< seed used to determine opmphmHashFunctionSeeds */
} OpmphmInit;

/**
 * Build functions
 */
int opmphmMapping (Opmphm * opmphm, OpmphmGraph * graph, OpmphmInit * init, size_t n);
int opmphmAssignment (Opmphm * opmphm, OpmphmGraph * graph, size_t n, int defaultOrder);

/**
 * Lookup functions
 */
size_t opmphmLookup (Opmphm * opmphm, const void * name);

/**
 * Basic functions
 */
Opmphm * opmphmNew (void);
void opmphmDel (Opmphm * opmphm);
void opmphmClear (Opmphm * opmphm);
int opmphmIsEmpty (Opmphm * opmphm);

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
