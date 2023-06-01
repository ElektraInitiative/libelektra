/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <internal/config.h>
#include <internal/core/opmphm.h>
#include <internal/kdbprivate.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/rand.h>

#include <string.h>

static int hasCycle (Opmphm * opmphm, OpmphmGraph * graph, size_t n);

/**
 * @brief Looks up a element in the OPMPHM.
 *
 * @param opmphm the OPMPHM
 * @param n the number of elements
 * @param name the name of the element
 *
 * @retval size_t the order of the element.
 */
size_t opmphmLookup (Opmphm * opmphm, size_t n, const void * name)
{
	ELEKTRA_NOT_NULL (opmphm);
	ELEKTRA_ASSERT (opmphm->rUniPar > 0 && opmphm->componentSize > 0, "passed opmphm is uninitialized");
	ELEKTRA_ASSERT (opmphm->graph != NULL, "passed opmphm is empty");
	ELEKTRA_ASSERT (name != NULL, "passed name is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	size_t ret = 0;
#ifndef OPMPHM_TEST
	size_t nameLength = strlen (name);
#endif
	for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
	{
#ifndef OPMPHM_TEST
		uint32_t hash = opmphmHashfunction (name, nameLength, opmphm->hashFunctionSeeds[r]) % opmphm->componentSize;
#else
		uint32_t hash = ((uint32_t *) name)[r];
#endif
		ret += opmphm->graph[r * opmphm->componentSize + hash];
	}
	return ret % n;
}

/**
 * @brief Assigns the vertices of the r-uniform r-partite hypergraph.
 *
 * Allocs the memory for the final OPMPHM `Opmphm->graph`.
 * Uses the remove sequence `OpmphmGraph->removeSequence`, generated during cycle check, to assign
 * each vertex. Either with `OpmphmEdge->order` or the default order, default is the order
 * of `OpmphmInit->data`.
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 * @param n the number of elements
 * @param defaultOrder boolean flag
 *
 * @retval 0 on success
 * @retval -1 on memory error
 */
int opmphmAssignment (Opmphm * opmphm, OpmphmGraph * graph, size_t n, int defaultOrder)
{
	ELEKTRA_NOT_NULL (opmphm);
	ELEKTRA_ASSERT (opmphm->rUniPar > 0 && opmphm->componentSize > 0, "passed opmphm is uninitialized");
	ELEKTRA_NOT_NULL (graph);
	ELEKTRA_ASSERT (graph->removeIndex == n, "graph contains a cycle");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	size_t size = opmphm->componentSize * opmphm->rUniPar * sizeof (uint32_t);
	opmphm->graph = elektraCalloc (size);
	if (!opmphm->graph)
	{
		return -1;
	}
	opmphm->size = size;
	// opmphm->graph[i] == 0 iff vertex i is not assigned
	// write 0 value with n, since n mod n = 0
	size_t i = n;
	do
	{
		--i;
		uint8_t assignableVertex = opmphm->rUniPar;
		size_t assignedValue = 0;
		// assign edge e
		uint32_t e = graph->removeSequence[i];
		for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
		{
			// go through all vertices from e
			size_t v = r * opmphm->componentSize + graph->edges[e].vertices[r];
			if (!opmphm->graph[v])
			{
				if (assignableVertex == opmphm->rUniPar)
				{
					// found the first assignableVertex
					assignableVertex = r;
				}
				else
				{
					// already found an assignableVertex
					// assign it != 0
					opmphm->graph[v] = 1;
					assignedValue += 1;
				}
			}
			else
			{
				assignedValue += opmphm->graph[v];
			}
		}
		ELEKTRA_ASSERT (assignableVertex != opmphm->rUniPar, "no assignableVertex, can not happen");
		// assing the assignableVertex
		size_t v = assignableVertex * opmphm->componentSize + graph->edges[e].vertices[assignableVertex];
		size_t order;
		if (defaultOrder)
		{
			order = e;
		}
		else
		{
			order = graph->edges[e].order;
		}
		if (assignedValue >= n)
		{
			assignedValue = assignedValue % n;
		}
		if (assignedValue <= order)
		{
			opmphm->graph[v] = (order - assignedValue) ? (order - assignedValue) : n;
		}
		else
		{
			opmphm->graph[v] = (n - assignedValue + order) ? (n - assignedValue + order) : n;
		}
	} while (i);
	return 0;
}

/**
 * @brief Maps the elements to edges in the r-uniform r-partite hypergraph.
 *
 * Sets the seeds for the opmphmHashfunctions, `OpmphmInit->initSeed` will be changed.
 * Inserts each element as edge in the r-uniform r-partite hypergraph and checks if the graph contains a cycle.
 * If there are cycles the `graph` will be cleaned
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 * @param init the OpmphmInit
 * @param n the number of elements
 *
 * @retval 0 on success
 * @retval -1 mapping not possible
 */
int opmphmMapping (Opmphm * opmphm, OpmphmGraph * graph, OpmphmInit * init, size_t n)
{
	ELEKTRA_NOT_NULL (opmphm);
	ELEKTRA_ASSERT (opmphm->rUniPar > 0 && opmphm->componentSize > 0, "passed opmphm is uninitialized");
	ELEKTRA_NOT_NULL (graph);
	ELEKTRA_NOT_NULL (init);
	ELEKTRA_ASSERT (init->getName != NULL, "passed init->getString is a Null Pointer");
	ELEKTRA_ASSERT (init->data != NULL, "passed init->data is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	// set seeds
	for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
	{
		elektraRand (&(init->initSeed));
		opmphm->hashFunctionSeeds[r] = init->initSeed;
	}
	for (size_t i = 0; i < n; ++i)
	{
#ifndef OPMPHM_TEST
		const char * name = init->getName (init->data[i]);
#endif
		for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
		{
#ifndef OPMPHM_TEST
			// set edge.h[]
			graph->edges[i].vertices[r] =
				opmphmHashfunction (name, strlen (name), opmphm->hashFunctionSeeds[r]) % opmphm->componentSize;
#endif
			// add edge to graph
			// set edge.nextEdge[r]
			size_t v = r * opmphm->componentSize + graph->edges[i].vertices[r];
			graph->edges[i].nextEdge[r] = graph->vertices[v].firstEdge;
			// set vertex.firstEdge
			graph->vertices[v].firstEdge = i;
			// increment degree
			++graph->vertices[v].degree;
		}
	}
	if (hasCycle (opmphm, graph, n))
	{
		// reset graph vertices
		opmphmGraphClear (opmphm, graph);
		return -1;
	}
	else
	{
		return 0;
	}
}

/**
 * @brief Recursive function used by hasCycle
 *
 * `v` is a degree 1 vertex with edge `e`. The edge `e` will be removed completely from the graph and
 * inserted in the `OpmphmGraph->removeSequence`.
 * For all vertices connected through `e` with degree 1 the function will be called again.
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 * @param v a vertex with degree 1
 */
static void peel_off (Opmphm * opmphm, OpmphmGraph * graph, size_t v)
{
	uint32_t e = graph->vertices[v].firstEdge;
	// add it to graph->removeSequence
	graph->removeSequence[graph->removeIndex] = e;
	++graph->removeIndex;
	// remove edge e from graph
	for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
	{
		// w is adjacent to v through e
		size_t w = r * opmphm->componentSize + graph->edges[e].vertices[r];
		// remove e from w
		uint32_t * j = &(graph->vertices[w].firstEdge);
		for (; *j != e; j = &(graph->edges[*j].nextEdge[r]))
			;
		*j = graph->edges[*j].nextEdge[r];
		// decrease degree
		--graph->vertices[w].degree;
	}
	// all vertices adjacent to v through e
	for (uint8_t r = 0; r < opmphm->rUniPar; ++r)
	{
		size_t w = r * opmphm->componentSize + graph->edges[e].vertices[r];
		// if degree 1, go on
		if (graph->vertices[w].degree == 1)
		{
			peel_off (opmphm, graph, w);
		}
	}
}

/**
 * @brief Checks if an OpmphmGraph is Acyclic
 *
 * Removes edges that have a degree 1 vertex, until the graph is empty.
 * The sequence of removed edges will be saved in `OpmphmGraph->removeSequence`.
 * The passed OpmphmGraph is will be destroyed.
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 * @param n the number of elements
 *
 * @retval 0 on Acyclic
 * @retval 1 if there is a cycle
 */
static int hasCycle (Opmphm * opmphm, OpmphmGraph * graph, size_t n)
{
	graph->removeIndex = 0;
	// search all vertices
	for (size_t v = 0; v < opmphm->componentSize * opmphm->rUniPar; ++v)
	{
		// for a vertex with degree 1
		if (graph->vertices[v].degree == 1)
		{
			peel_off (opmphm, graph, v);
		}
	}
	if (graph->removeIndex == n)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

/**
 * @brief Provides the minimal `c` value for a given `r`
 *
 * This minimal values come from Fabiano Cupertino Botelho, Near-Optimal Space Perfect Hashing Algorithms, 2008.
 *
 * @param r the rUniPar
 *
 * @retval c the minimal c value
 */
double opmphmMinC (uint8_t r)
{
	ELEKTRA_ASSERT (r > 1 && r < 11, "r out of range [2,10]");
	switch (r)
	{
	case 2:
		return 2.05;
	case 3:
		return 1.25;
	case 4:
		return 1.35;
	case 5:
		return 1.45;
	case 6:
		return 1.65;
	case 7:
		return 1.75;
	case 8:
		return 1.95;
	case 9:
		return 2.05;
	case 10:
		return 2.25;
	default:
		return 0;
	}
}

/**
 * @brief Provides the optimal `r` value for a given `n`
 *
 * This is a heuristic, the return values follow from the mapping benchmark.
 *
 * @param n the number of elements to hash
 *
 * @retval r the optimal rUniPar
 */
uint8_t opmphmOptR (size_t n)
{
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	if (n < 15)
	{
		return 6;
	}
	else if (n < 30)
	{
		return 5;
	}
	else if (n < 240)
	{
		return 4;
	}
	else
	{
		return 3;
	}
}

/**
 * @brief Provides the optimal `c` value for a given `n`
 *
 * This is a heuristic, the return values follow from the mapping benchmark.
 *
 * @param n the number of elements to hash
 *
 * @retval c the optimal `c` value
 */
double opmphmOptC (size_t n)
{
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	if (n < 15)
	{
		return 1.35;
	}
	else if (n < 30)
	{
		//~ from 15 to 29 from 2.45 to 1.95
		return 1 - ((n - 15) * 0.035);
	}
	else if (n < 240)
	{
		//~ from 30 to 239 from 2.35 to 1.45
		return 1 - ((n - 30) * 0.0043);
	}
	else if (n < 1280)
	{
		//~ from 240 to 1279 from 2.25 to 1.35
		return 1 - ((n - 240) * 0.00086);
	}
	else
	{
		return 0.1;
	}
}

/**
 * @brief Allocates and initializes the OpmphmGraph.
 *
 * The OpmphmGraph represents a r-uniform r-partite hypergraph.
 * Lazy initializes the `opmphm->hashFunctionSeeds` with r.
 * Calculates also the size of one partition in the r-uniform r-partite hypergraph and stores it in `opmphm->componentSize`.
 * Allocates all memory for the OpmphmGraph.
 *
 * @param opmphm the OPMPHM
 * @param r the rUniPar
 * @param n the number of elements
 * @param c space influencing parameter
 *
 * @retval OpmphmGraph * success
 * @retval NULL memory error
 */
OpmphmGraph * opmphmGraphNew (Opmphm * opmphm, uint8_t r, size_t n, double c)
{
	ELEKTRA_NOT_NULL (opmphm);
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (n <= KDB_OPMPHM_MAX_N, "n > KDB_OPMPHM_MAX");
	ELEKTRA_ASSERT (c > 0.0, "ratio <= 0");
	ELEKTRA_ASSERT (1 < r && r < 11, "r out of range [2,10]");
	// lazy create
	if (r != opmphm->rUniPar)
	{
		// free if here
		if (opmphm->rUniPar)
		{
			if (!test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS)) elektraFree (opmphm->hashFunctionSeeds);
		}
		opmphm->hashFunctionSeeds = elektraMalloc (r * sizeof (int32_t));
		clear_bit (opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS);
		if (!opmphm->hashFunctionSeeds)
		{
			return NULL;
		}
		opmphm->rUniPar = r;
	}
	// calculate opmphm->componentSize, number of elements in one part of r-uniform r-partite hypergraph
	opmphm->componentSize = (c * n / opmphm->rUniPar) + 1;
	// mallocs
	OpmphmGraph * graph = elektraMalloc (sizeof (OpmphmGraph));
	if (!graph)
	{
		opmphm->componentSize = 0;
		return NULL;
	}
	graph->edges = elektraMalloc (n * sizeof (OpmphmEdge));
	if (!graph->edges)
	{
		opmphm->componentSize = 0;
		elektraFree (graph);
		return NULL;
	}
	graph->vertices = elektraCalloc (opmphm->componentSize * opmphm->rUniPar * sizeof (OpmphmVertex));
	if (!graph->vertices)
	{
		opmphm->componentSize = 0;
		elektraFree (graph->edges);
		elektraFree (graph);
		return NULL;
	}
	/* one malloc for:
	 * - graph->removeSequence	n
	 * - graph->edges[i].vertices	n * opmphm->rUniPar
	 * - graph->edges[i].nextEdge	n * opmphm->rUniPar
	 */
	uint32_t * removeSequenceVerticesNextEdge = elektraMalloc ((n + 2 * n * opmphm->rUniPar) * sizeof (uint32_t));
	if (!removeSequenceVerticesNextEdge)
	{
		opmphm->componentSize = 0;
		elektraFree (graph->vertices);
		elektraFree (graph->edges);
		elektraFree (graph);
		return NULL;
	}
	// split removeSequenceVerticesNextEdge for graph->removeSequence, and for graph->edges[].vertices and graph->edges[].nextEdge
	graph->removeSequence = &removeSequenceVerticesNextEdge[0];
	for (size_t i = 0; i < n; ++i)
	{
		graph->edges[i].vertices = &removeSequenceVerticesNextEdge[n + i * opmphm->rUniPar];
		graph->edges[i].nextEdge = &removeSequenceVerticesNextEdge[(n + opmphm->rUniPar * n) + i * opmphm->rUniPar];
	}
	return graph;
}

/**
 * @brief Clears the OpmphmGraph.
 *
 * Sets all vertices to 0.
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 */
void opmphmGraphClear (const Opmphm * opmphm, OpmphmGraph * graph)
{
	ELEKTRA_NOT_NULL (opmphm);
	ELEKTRA_ASSERT (opmphm->rUniPar > 0 && opmphm->componentSize > 0, "passed opmphm is uninitialized");
	ELEKTRA_NOT_NULL (graph);
	memset (graph->vertices, 0, opmphm->componentSize * opmphm->rUniPar * sizeof (OpmphmVertex));
}

/**
 * @brief Deletes the OpmphmGraph.
 *
 * @param graph the OpmphmGraph
 */
void opmphmGraphDel (OpmphmGraph * graph)
{
	ELEKTRA_NOT_NULL (graph);
	elektraFree (graph->removeSequence);
	elektraFree (graph->edges);
	elektraFree (graph->vertices);
	elektraFree (graph);
}

/**
 * @brief Allocates and initializes the OPMPHM.
 *
 * @retval Opmphm * success
 * @retval NULL memory error
 */
Opmphm * opmphmNew (void)
{
	return elektraCalloc (sizeof (Opmphm));
}

/**
 * @brief Copies OPMPHM from source to destination.
 *
 * Clears the dest and copies memory and values from source.
 *
 * @param dest the OPMPHM destination
 * @param source the OPMPHM source
 *
 * @retval 0 on success
 * @retval -1 on memory error
 */
int opmphmCopy (Opmphm * dest, const Opmphm * source)
{
	ELEKTRA_NOT_NULL (dest);
	ELEKTRA_NOT_NULL (source);
	// reset dest
	opmphmClear (dest);
	if (dest->rUniPar)
	{
		if (!test_bit (dest->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS)) elektraFree (dest->hashFunctionSeeds);
		clear_bit (dest->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS);
		dest->rUniPar = 0;
	}
	dest->componentSize = 0;

	// copy mem
	if (source->rUniPar)
	{
		dest->hashFunctionSeeds = elektraMalloc (source->rUniPar * sizeof (int32_t));
		if (!dest->hashFunctionSeeds)
		{
			return -1;
		}
		memcpy (dest->hashFunctionSeeds, source->hashFunctionSeeds, source->rUniPar * sizeof (int32_t));
	}
	if (source->size)
	{
		dest->graph = elektraMalloc (source->size);
		if (!dest->graph)
		{
			elektraFree (dest->hashFunctionSeeds);
			return -1;
		}
		memcpy (dest->graph, source->graph, source->size);
	}

	// copy values
	dest->componentSize = source->componentSize;
	dest->rUniPar = source->rUniPar;
	dest->size = source->size;
	return 0;
}

/**
 * @brief OPMPHM is build.
 *
 * @param opmphm the OPMPHM
 *
 * @retval 0 on false
 * @retval -1 on true or NULL
 */
int opmphmIsBuild (const Opmphm * opmphm)
{
	if (opmphm && opmphm->size)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

/**
 * @brief Deletes the OPMPHM.
 *
 * Clears and frees all memory in Opmphm.
 *
 * @param opmphm the OPMPHM
 */
void opmphmDel (Opmphm * opmphm)
{
	ELEKTRA_NOT_NULL (opmphm);
	opmphmClear (opmphm);
	// omit clearing mmap flags since complete structure is free()d
	if (opmphm->rUniPar && !test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS))
	{
		elektraFree (opmphm->hashFunctionSeeds);
	}
	if (!test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_STRUCT)) elektraFree (opmphm);
}

/**
 * @brief Clears the OPMPHM.
 *
 * The OPMPHM must be successfully created with `opmphmNew ()`.
 * Clears and frees all internal memory of Opmphm, but not `Opmphm->hashFunctionSeeds` and the Opmphm instance.
 *
 * @param opmphm the OPMPHM
 */
void opmphmClear (Opmphm * opmphm)
{
	ELEKTRA_NOT_NULL (opmphm);
	if (opmphmIsBuild (opmphm))
	{
		if (!test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_GRAPH)) elektraFree (opmphm->graph);
		clear_bit (opmphm->flags, OPMPHM_FLAG_MMAP_GRAPH);
		opmphm->size = 0;
	}
}

/**
 * Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashlitte
 */
#ifndef ELEKTRA_BIG_ENDIAN
// little endian
// sanitize a hash function is silly, so ignore it!
ELEKTRA_NO_SANITIZE_UNDEFINED
ELEKTRA_NO_SANITIZE_INTEGER
ELEKTRA_NO_SANITIZE_ADDRESS
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;
	a = b = c = 0xdeadbeef + ((uint32_t) length) + initval;
	const uint32_t * k = (const uint32_t *) key;
	while (length > 12)
	{
		a += k[0];
		b += k[1];
		c += k[2];
		OPMPHM_HASHFUNCTION_MIX (a, b, c)
		length -= 12;
		k += 3;
	}
	switch (length)
	{
	case 12:
		c += k[2];
		b += k[1];
		a += k[0];
		break;
	case 11:
		c += k[2] & 0xffffff;
		b += k[1];
		a += k[0];
		break;
	case 10:
		c += k[2] & 0xffff;
		b += k[1];
		a += k[0];
		break;
	case 9:
		c += k[2] & 0xff;
		b += k[1];
		a += k[0];
		break;
	case 8:
		b += k[1];
		a += k[0];
		break;
	case 7:
		b += k[1] & 0xffffff;
		a += k[0];
		break;
	case 6:
		b += k[1] & 0xffff;
		a += k[0];
		break;
	case 5:
		b += k[1] & 0xff;
		a += k[0];
		break;
	case 4:
		a += k[0];
		break;
	case 3:
		a += k[0] & 0xffffff;
		break;
	case 2:
		a += k[0] & 0xffff;
		break;
	case 1:
		a += k[0] & 0xff;
		break;
	case 0:
		return c;
	}
	OPMPHM_HASHFUNCTION_FINAL (a, b, c);
	return c;
}
#else
// big endian
// sanitize a hash function is silly, so ignore it!
ELEKTRA_NO_SANITIZE_UNDEFINED
ELEKTRA_NO_SANITIZE_INTEGER
ELEKTRA_NO_SANITIZE_ADDRESS
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;
	a = b = c = 0xdeadbeef + ((uint32_t) length) + initval;
	const uint8_t * k = (const uint8_t *) key;
	while (length > 12)
	{
		a += k[0];
		a += ((uint32_t) k[1]) << 8;
		a += ((uint32_t) k[2]) << 16;
		a += ((uint32_t) k[3]) << 24;
		b += k[4];
		b += ((uint32_t) k[5]) << 8;
		b += ((uint32_t) k[6]) << 16;
		b += ((uint32_t) k[7]) << 24;
		c += k[8];
		c += ((uint32_t) k[9]) << 8;
		c += ((uint32_t) k[10]) << 16;
		c += ((uint32_t) k[11]) << 24;
		OPMPHM_HASHFUNCTION_MIX (a, b, c);
		length -= 12;
		k += 12;
	}
	switch (length)
	{
	case 12:
		c += ((uint32_t) k[11]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 11:
		c += ((uint32_t) k[10]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 10:
		c += ((uint32_t) k[9]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 9:
		c += k[8];
		ELEKTRA_FALLTHROUGH;
	case 8:
		b += ((uint32_t) k[7]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 7:
		b += ((uint32_t) k[6]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 6:
		b += ((uint32_t) k[5]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 5:
		b += k[4];
		ELEKTRA_FALLTHROUGH;
	case 4:
		a += ((uint32_t) k[3]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 3:
		a += ((uint32_t) k[2]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 2:
		a += ((uint32_t) k[1]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 1:
		a += k[0];
		break;
	case 0:
		return c;
	}
	OPMPHM_HASHFUNCTION_FINAL (a, b, c);
	return c;
}
#endif
