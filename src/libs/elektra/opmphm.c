/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbopmphm.h>
#include <kdbrand.h>

#include <string.h>

static int hasCycle (Opmphm * opmphm, OpmphmGraph * graph, size_t n);

/**
 * @brief Looks up a element in the OPMPHM.
 *
 * @param opmphm the OPMPHM
 * @param name the name of the element
 *
 * @retval size_t the order of the element.
 */
size_t opmphmLookup (Opmphm * opmphm, const void * name)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (opmphm->graph != NULL, "passed opmphm is empty");
	ELEKTRA_ASSERT (name != NULL, "passed name is a Null Pointer");
	size_t ret = 0;
#ifndef OPMPHM_TEST
	size_t nameLength = strlen (name);
#endif
	for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
	{
#ifndef OPMPHM_TEST
		uint32_t hash = opmphmHashfunction (name, nameLength, opmphm->opmphmHashFunctionSeeds[r]) % opmphm->componentSize;
#else
		uint32_t hash = ((uint32_t *)name)[r];
#endif
		ret += opmphm->graph[r * opmphm->componentSize + hash];
	}
	return ret % (opmphm->componentSize * OPMPHMR_PARTITE);
}

/**
 * @brief Assigns the vertices of the r-partite hypergraph.
 *
 * Allocs the memory for the final OPMPHM `Opmphm->graph`.
 * Uses the remove sequence `OpmphmGraph->removeOrder`, generated during cycle check, to assign
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
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (graph != NULL, "passed graph is a Null Pointer");
	ELEKTRA_ASSERT (graph->removeIndex == n, "graph contains a cycle");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	opmphm->size = opmphm->componentSize * OPMPHMR_PARTITE * sizeof (size_t);
	opmphm->graph = elektraCalloc (opmphm->size);
	if (!opmphm->graph)
	{
		return -1;
	}
	// malloc assignment flag for each vertex
	uint8_t * isAssigned = elektraCalloc (opmphm->componentSize * OPMPHMR_PARTITE * sizeof (uint8_t));
	if (!isAssigned)
	{
		elektraFree (opmphm->graph);
		opmphm->size = 0;
		return -1;
	}
	for (ssize_t i = n - 1; i >= 0; --i)
	{
		// assign edge e
		// find out how many vertices are assigned and what is the sum of the values
		uint8_t notAssignedCount = 0;
		ssize_t assignedValue = 0;
		size_t e = graph->removeOrder[i];
		for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
		{
			size_t v = r * opmphm->componentSize + graph->edges[e].h[r];
			if (!isAssigned[v])
			{
				++notAssignedCount;
			}
			else
			{
				assignedValue += opmphm->graph[v];
			}
		}
		ELEKTRA_ASSERT (notAssignedCount != 0, "no not assigned vertex");
		// set all but one to assigned state
		uint8_t r = 0;
		while (notAssignedCount > 1)
		{
			size_t v = r * opmphm->componentSize + graph->edges[e].h[r];
			if (!isAssigned[v])
			{
				isAssigned[v] = 1;
				--notAssignedCount;
			}
			++r;
		}
		// give the last the desired order
		for (; r < OPMPHMR_PARTITE; ++r)
		{
			size_t v = r * opmphm->componentSize + graph->edges[e].h[r];
			if (!isAssigned[v])
			{
				ssize_t diff;
				if (defaultOrder)
				{
					diff = ((ssize_t)e - assignedValue) % (ssize_t) (opmphm->componentSize * OPMPHMR_PARTITE);
				}
				else
				{
					diff = ((ssize_t)graph->edges[e].order - assignedValue) %
					       (ssize_t) (opmphm->componentSize * OPMPHMR_PARTITE);
				}
				if (diff < 0)
				{
					diff += opmphm->componentSize * OPMPHMR_PARTITE;
				}
				opmphm->graph[v] = diff;
				isAssigned[v] = 1;
			}
		}
	}
	elektraFree (isAssigned);
	return 0;
}

/**
 * @brief Maps the elements to edges in a r-partite hypergraph.
 *
 * Sets the seeds for the opmphmHashfunctions, `OpmphmInit->initSeed` will be changed.
 * Inserts each element as edge in the r-partite hypergraph and checks if the graph contains a cycle.
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
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (graph != NULL, "passed graph is a Null Pointer");
	ELEKTRA_ASSERT (init != NULL, "passed init is a Null Pointer");
	ELEKTRA_ASSERT (init->getString != NULL, "passed init->getString is a Null Pointer");
	ELEKTRA_ASSERT (init->data != NULL, "passed init->data is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	// set seeds
	for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
	{
		elektraRand (&(init->initSeed));
		opmphm->opmphmHashFunctionSeeds[r] = init->initSeed;
	}
	for (size_t i = 0; i < n; ++i)
	{
#ifndef OPMPHM_TEST
		const char * name = init->getString (init->data[i]);
#endif
		for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
		{
#ifndef OPMPHM_TEST
			// set edge.h[]
			graph->edges[i].h[r] =
				opmphmHashfunction (name, strlen (name), opmphm->opmphmHashFunctionSeeds[r]) % opmphm->componentSize;
#endif
			// add edge to graph
			// set edge.nextEdge[r]
			size_t v = r * opmphm->componentSize + graph->edges[i].h[r];
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
		memset (graph->vertices, 0, opmphm->componentSize * OPMPHMR_PARTITE * sizeof (OpmphmVertex));
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
 * inserted in the `OpmphmGraph->removeOrder`.
 * For all vertices connected through `e` with degree 1 the function will be called again.
 *
 * @param opmphm the OPMPHM
 * @param graph the OpmphmGraph
 * @param v a vertex with degree 1
 */
static void peel_off (Opmphm * opmphm, OpmphmGraph * graph, size_t v)
{
	size_t e = graph->vertices[v].firstEdge;
	// add it to graph->removeOrder
	graph->removeOrder[graph->removeIndex] = e;
	++graph->removeIndex;
	// remove edge e from graph
	for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
	{
		// w is adjacent to v through e
		size_t w = r * opmphm->componentSize + graph->edges[e].h[r];
		// remove e from w
		size_t * j = &(graph->vertices[w].firstEdge);
		for (; *j != e; j = &(graph->edges[*j].nextEdge[r]))
			;
		*j = graph->edges[*j].nextEdge[r];
		// decrease degree
		--graph->vertices[w].degree;
	}
	// all vertices adjacent to v through e
	for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
	{
		size_t w = r * opmphm->componentSize + graph->edges[e].h[r];
		// if degree 1, go on
		if (graph->vertices[w].degree == 1)
		{
			peel_off (opmphm, graph, w);
		}
	}
}

/**
 * @brief Checks if a OpmphmGraph is Acyclic
 *
 * Removes edges that have a degree 1 vertex, until the graph is empty.
 * The sequence of removed edges will be saved in `OpmphmGraph->removeOrder`.
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
	for (size_t v = 0; v < opmphm->componentSize * OPMPHMR_PARTITE; ++v)
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
 * @brief Provides the minimal c value
 *
 * This minimal values come from Fabiano Cupertino Botelho, Near-Optimal Space Perfect Hashing Algorithms, 2008.
 *
 * @retval c the minimal c value
 */
double opmphmMinC (void)
{
	ELEKTRA_ASSERT (OPMPHMR_PARTITE > 1 && OPMPHMR_PARTITE < 11, "OPMPHMR_PARTITE out of range");
	switch (OPMPHMR_PARTITE)
	{
	case 2:
		return 2.01;
	case 3:
		return 1.24;
	case 4:
		return 1.31;
	case 5:
		return 1.45;
	case 6:
		return 1.6;
	case 7:
		return 1.75;
	case 8:
		return 1.9;
	case 9:
		return 2.05;
	case 10:
		return 2.2;
	default:
		return 0;
	}
}

/**
 * @brief Allocates and initializes the OpmphmGraph.
 *
 * The OpmphmGraph represents a r-partite hypergraph.
 * Calculates also the size of one partition in the r-partite hypergraph and stores it in `opmphm->componentSize`.
 *
 * @param opmphm the OPMPHM
 * @param n the number of elements
 * @param c space influencing parameter
 *
 * @retval OpmphmGraph * success
 * @retval NULL memory error
 */
OpmphmGraph * opmphmGraphNew (Opmphm * opmphm, size_t n, double c)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "n is 0");
	ELEKTRA_ASSERT (c > 0.0, "ratio <= 0");
	// calculate opmphm->componentSize, number of elements in one part of r-partite hypergraph
	opmphm->componentSize = (c * n / OPMPHMR_PARTITE) + 1;
	// mallocs
	OpmphmGraph * graph = elektraMalloc (sizeof (OpmphmGraph));
	if (!graph)
	{
		return NULL;
	}
	graph->edges = elektraMalloc (n * sizeof (OpmphmEdge));
	if (!graph->edges)
	{
		elektraFree (graph);
		return NULL;
	}
	graph->removeOrder = elektraMalloc (n * sizeof (size_t));
	if (!graph->edges)
	{
		elektraFree (graph->removeOrder);
		elektraFree (graph);
		return NULL;
	}
	graph->vertices = elektraCalloc (opmphm->componentSize * OPMPHMR_PARTITE * sizeof (OpmphmVertex));
	if (!graph->vertices)
	{
		elektraFree (graph->removeOrder);
		elektraFree (graph->vertices);
		elektraFree (graph);
		return NULL;
	}
	return graph;
}


/**
 * @brief Deletes the OpmphmGraph.
 *
 * @param graph the OpmphmGraph
 */
void opmphmGraphDel (OpmphmGraph * graph)
{
	ELEKTRA_ASSERT (graph != NULL, "passed graph is a Null Pointer");
	elektraFree (graph->edges);
	elektraFree (graph->removeOrder);
	elektraFree (graph->vertices);
	elektraFree (graph);
}

/**
 * @brief Allocates and initializes the OPMPHM.
 *
 * The returned OPMPHM instance is Empty.
 *
 * @retval Opmphm * success
 * @retval NULL memory error
 */
Opmphm * opmphmNew (void)
{
	Opmphm * out = elektraMalloc (sizeof (Opmphm));
	if (!out)
	{
		return NULL;
	}
	out->size = 0;
	return out;
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
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	opmphmClear (opmphm);
	elektraFree (opmphm);
}

/**
 * @brief Clears the OPMPHM.
 *
 * Clears and frees all internal memory of Opmphm, but not the Opmphm instance.
 *
 * @param opmphm the OPMPHM
 */
void opmphmClear (Opmphm * opmphm)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	if (!opmphmIsEmpty (opmphm))
	{
		elektraFree (opmphm->graph);
		opmphm->size = 0;
	}
}

/**
 * @brief Determines if the OPMPHM is Empty.
 *
 * Empty means opmphm->size is 0.
 *
 * @param opmphm the OPMPHM
 *
 * @retval true empty
 * @retval false non empty

 */
int opmphmIsEmpty (Opmphm * opmphm)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	if (opmphm->size)
		return 0;
	else
		return 1;
}

/**
 * Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashlitte
 */
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;
	a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;
#ifndef ELEKTRA_BIG_ENDIAN
	// little endian
	const uint32_t * k = (const uint32_t *)key;
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
#else
	// big endian
	const uint8_t * k = (const uint8_t *)key;
	while (length > 12)
	{
		a += k[0];
		a += ((uint32_t)k[1]) << 8;
		a += ((uint32_t)k[2]) << 16;
		a += ((uint32_t)k[3]) << 24;
		b += k[4];
		b += ((uint32_t)k[5]) << 8;
		b += ((uint32_t)k[6]) << 16;
		b += ((uint32_t)k[7]) << 24;
		c += k[8];
		c += ((uint32_t)k[9]) << 8;
		c += ((uint32_t)k[10]) << 16;
		c += ((uint32_t)k[11]) << 24;
		OPMPHM_HASHFUNCTION_MIX (a, b, c);
		length -= 12;
		k += 12;
	}
	switch (length)
	{
	case 12:
		c += ((uint32_t)k[11]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 11:
		c += ((uint32_t)k[10]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 10:
		c += ((uint32_t)k[9]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 9:
		c += k[8];
		ELEKTRA_FALLTHROUGH;
	case 8:
		b += ((uint32_t)k[7]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 7:
		b += ((uint32_t)k[6]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 6:
		b += ((uint32_t)k[5]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 5:
		b += k[4];
		ELEKTRA_FALLTHROUGH;
	case 4:
		a += ((uint32_t)k[3]) << 24;
		ELEKTRA_FALLTHROUGH;
	case 3:
		a += ((uint32_t)k[2]) << 16;
		ELEKTRA_FALLTHROUGH;
	case 2:
		a += ((uint32_t)k[1]) << 8;
		ELEKTRA_FALLTHROUGH;
	case 1:
		a += k[0];
		break;
	case 0:
		return c;
	}
#endif
	OPMPHM_HASHFUNCTION_FINAL (a, b, c);
	return c;
}
