#ifndef OPMPHM_H
#define OPMPHM_H

#include <stdint.h>
#include <stdlib.h>

/**
 * Vertex represents the vertex of the bipartite graph.
 * Only needed for build, 2*r times (see opmphmR, below).
 */
typedef struct
{
	int firstEdge; /*!< index of the fist edge in the list */
	size_t degree; /*!< number of edges in the list */
} Vertex;

/**
 * Edge represents the edge of the bipartite graph and is also the key.
 * Only needed for build, number of keys times.
 */
typedef struct
{
	uint32_t h[3];   /*!< the hash function value of each key */
	int nextEdge[2]; /*!< the index of the next edge in the edge list of the vertex */
	size_t order;    /*!< the desired index of the key in the opmphm hash map */
} Edge;

/**
 * The vertices and edges represent the bipartite graph.
 *
 * Each Vertex has a list, each Edge is exactly in two lists.
 * The next element of each list is defined in nextEdge.
 * nextEdge[1] is for the lists on one side of the bipartite graph (0 to r -1)
 * nextEdge[2] is for the lists in the other side of the bipartite graph (r to 2*r-1)
 */


/**
 * Only needed for Initialisation, during Mapping
 */
typedef const char * (*opmphmGetString) (void *);
typedef struct
{
	opmphmGetString getString; /*!< Function pointer used to extract the key name from the data. */
	void ** data;		   /*!< The data */
	uint32_t seed;		   /*!< The seed for random actions */
} OPMPHMinit;


/**
 * OPMPHM represents the final order preserving hash map.
 * This struct is needed for the lookup, during the lookup the string gets hashed 3 times
 * with different seeds from opmphmHashFunctionSeeds, the choice of the actual hash function depends on the mark bit
 * and a purely saved function g: [0 ...  2 * r] -> n, the core of the OPMPHM, is used.
 */
typedef struct
{
	uint32_t opmphmHashFunctionSeeds[3]; /*!< the seeds for the tree hash function calls */
	unsigned int * g;		     /*!< saves the g () function need for the lookup and filled while the search phase */
	uint8_t * mark;			     /*!< saves boolean values, to mark the indirection */
} OPMPHM;

/**
 * The ratio defines the number of vertices on one side of the biparithegraph, denoted as r.
 * Double this value to get to the Fox et al. ratio.
 */
extern double opmphmRatio;

/** The tree phases */
int opmphmMapping (OPMPHM * opmphm, Vertex * vertices, Edge * edges, OPMPHMinit * init, size_t n);

/* Debug */
void opmphmPrintGraph (Edge * edges, void ** data, opmphmGetString fpOpmhpmGetString, size_t n);

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

/* Random */

uint32_t opmphmRandom (unsigned int * seedp);

/**
 * The requirements are described in doc/help/elektra-data-structures.md.
 */

/**
* @defgroup datastructs Datastructures
* @brief Datastructures
*/


/**
 *
 * @defgroup vstack Vstack
 * @ingroup datastructs
 *
 * The Vstack structure.
 *
 * A stack implementation with dynamical memory allocation.
 * The space gets doubled if full and reduced by half if used only a quarter of it,
 * but not less than minSize.
 */

typedef struct
{
	size_t minSize; /*!< the minimal size of the stack > */
	size_t size;    /*!< The maximal size of the stack */
	void ** data;   /*!< The data array */
	void ** head;   /*!< The stack pointer, which gets incremented or decremented after each push or pop. */
} Vstack;

Vstack * elektraVstackInit (size_t minSize);
int elektraVstackPush (Vstack * stack, void * data);
void * elektraVstackPop (Vstack * stack);
int elektraVstackIsEmpty (const Vstack * stack);
void elektraVstackDel (Vstack * stack);
int elektraVstackClear (Vstack * stack);


typedef int (*VheapComp) (void *, void *);
/**
 *
 * @defgroup vheap Vheap
 * @ingroup datastructs
 *
 * The Vheap structure.
 *
 * A heap is a Data structure which keeps the data ordered.
 * Elements can only be retrieved in this order! Insertions and retrieves need log (n).
 *
 * This implementation allocates memory dynamically.
 * The space gets doubled if full and reduced by half if used only a quarter of it,
 * but not less than minSize.
 *
 * To construct a max heap the comparison function VheapComp (a, b), must return
 * 1 on a > b and 0 otherwise. For a min heap 1 on a < b and 0 otherwise.
 */
typedef struct
{
	size_t minSize; /*!< the minimal size of the heap > */
	size_t size;    /*!< The size of the heap */
	size_t count;   /*!< The current number of elements in the heap */
	VheapComp comp; /*!< The comparison function */
	void ** data;   /*!< The data array */
} Vheap;

Vheap * elektraVheapInit (VheapComp comp, size_t minSize);
void elektraVheapDel (Vheap * vheap);
int elektraVheapClear (Vheap * vheap);
int elektraVheapIsEmpty (const Vheap * vheap);
int elektraVheapInsert (Vheap * vheap, void * data);
void * elektraVheapRemove (Vheap * vheap);

#endif
