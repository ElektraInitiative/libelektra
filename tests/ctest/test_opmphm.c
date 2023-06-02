/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#define OPMPHM_TEST
#include "../../src/libs/elektra/opmphm.c"

#include <tests_internal.h>

// dummy for testing
static const char * test_opmphm_getName (void * data ELEKTRA_UNUSED)
{
	return "Test Key Name";
}

const size_t maxComponentSize = 273;
const uint8_t minRUniPar = 2;
const uint8_t maxRUniPar = 10;

static void test_basic (void)
{
	Opmphm * opmphm = opmphmNew ();
	exit_if_fail (opmphm, "opmphmNew");
	succeed_if (!opmphmIsBuild (opmphm), "opmphm is build but should be not build");
	succeed_if (!opmphmIsBuild (NULL), "isBuild NULL returned true");
	opmphm->size = 1;
	succeed_if (opmphmIsBuild (opmphm), "opmphm is not build but should be build");
	opmphm->size = 0;
	opmphmDel (opmphm);
}

static void test_opmphmGraphNew (void)
{
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 1; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / rUniPar) + 1 <= componentSize; ++n)
				;

			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			// check opmphm
			succeed_if (opmphm->rUniPar == 0, "check opmphm->rUniPar");
			succeed_if (opmphm->size == 0, "check opmphm->size");
			succeed_if (opmphm->componentSize == 0, "check opmphm->componentSize");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			// check opmphm
			succeed_if (opmphm->rUniPar == rUniPar, "check opmphm->rUniPar");
			succeed_if (opmphm->componentSize == componentSize, "check opmphm->componentSize");
			// check access Opmphm->hashFunctionSeeds
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				opmphm->hashFunctionSeeds[r] = 0;
				succeed_if (opmphm->hashFunctionSeeds[r] == 0, "check access opmphm->hashFunctionSeeds");
			}
			// check access OpmphmEdge->vertices ,OpmphmEdge->nextEdge and graph->removeSequence
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = i * r;
					graph->edges[i].nextEdge[r] = i * r;
				}
				graph->removeSequence[i] = i;
			}
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					succeed_if (graph->edges[i].vertices[r] == i * r, "check access OpmphmEdge->vertices");
					succeed_if (graph->edges[i].nextEdge[r] == i * r, "check access OpmphmEdge->nextEdge");
				}
				succeed_if (graph->removeSequence[i] == i, "check access graph->removeSequence");
			}
			// check vertices initialization
			for (size_t i = 0; i < componentSize * rUniPar; ++i)
			{
				succeed_if (graph->vertices[i].firstEdge == 0, "check vertices initialization");
				succeed_if (graph->vertices[i].degree == 0, "check vertices initialization");
			}

			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}
static void test_minComponentSize (void)
{
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		Opmphm * opmphm = opmphmNew ();
		exit_if_fail (opmphm, "opmphmNew");
		OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, 1, 1);
		exit_if_fail (graph, "opmphmGraphNew");
		// check
		succeed_if (1 == opmphm->componentSize, "min componentSize is not 1");
		// cleanup
		opmphmDel (opmphm);
		opmphmGraphDel (graph);
	}
}


static void test_cyclicMultipleEdges (void)
{
	/**
	 * Test multiple edges, there is at least one edge multiple times
	 */
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 2; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / rUniPar) + 1 <= componentSize; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			OpmphmInit opmphmInit;
			// dummy data
			opmphmInit.getName = test_opmphm_getName;
			opmphmInit.initSeed = (int32_t) 1;
			opmphmInit.data = (void **) 1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = i % componentSize;
				}
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &opmphmInit, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}
static void test_cyclicCountUpEdges (void)
{
	/**
	 * Test cycles, fill up edge.h[] with all possible edges, starting from (0,...,0), no multiple edges.
	 */
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 3; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / rUniPar) + 1 <= componentSize; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			OpmphmInit opmphmInit;
			// dummy data
			opmphmInit.getName = test_opmphm_getName;
			opmphmInit.initSeed = (int32_t) 1;
			opmphmInit.data = (void **) 1;
			// fill
			uint32_t data[rUniPar];
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				data[r] = 0;
			}
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = data[r];
				}
				// count up data
				uint8_t r = rUniPar;
				do
				{
					--r;
					data[r] = (data[r] + 1) % componentSize;
				} while (data[r] == 0);
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &opmphmInit, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}
static void test_cyclicCountDownEdges (void)
{
	/**
	 * Test cycles, fill up edge.h[] with all possible edges, starting from (p - 1,...,p - 1), no multiple edges.
	 */
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 3; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / rUniPar) + 1 <= componentSize; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			OpmphmInit opmphmInit;
			// dummy data
			opmphmInit.getName = test_opmphm_getName;
			opmphmInit.initSeed = (int32_t) 1;
			opmphmInit.data = (void **) 1;
			// fill
			int32_t data[rUniPar];
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				data[r] = componentSize - 1;
			}
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = data[r];
				}
				// count down data
				uint8_t r = rUniPar;
				do
				{
					--r;
					--data[r];
					if (data[r] < 0)
					{
						data[r] = componentSize - 1;
					}
				} while (data[r] == (int32_t) componentSize - 1);
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &opmphmInit, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}


static void test_acyclicDefaultOrder (void)
{
	/**
	 * Test:
	 * * Empty and Clear
	 * * reuse of mapping
	 * * assignment (default order)
	 * * lookup
	 */
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 3; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n = componentSize - 1;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, rUniPar);
			exit_if_fail (graph, "opmphmGraphNew");
			OpmphmInit opmphmInit;
			// dummy data
			opmphmInit.getName = test_opmphm_getName;
			opmphmInit.initSeed = (int32_t) 1;
			opmphmInit.data = (void **) 1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = (i + r) % componentSize;
				}
			}
			// save element last and create multiple edge
			uint32_t data[rUniPar];
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				data[r] = graph->edges[n - 1].vertices[r];
				graph->edges[n - 1].vertices[r] = graph->edges[0].vertices[r];
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &opmphmInit, n), "graph with cycles marked as acyclic");
			// restore last element
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				graph->edges[n - 1].vertices[r] = data[r];
			}
			succeed_if (!opmphmMapping (opmphm, graph, &opmphmInit, n), "acyclic graph marked as cyclic");
			exit_if_fail (opmphmAssignment (opmphm, graph, n, 1) == 0, "opmphmAssignment");
			for (size_t i = 0; i < n; ++i)
			{
				succeed_if (opmphmLookup (opmphm, n, graph->edges[i].vertices) == i, "lookup");
			}
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}
static void test_acyclicReverseOrder (void)
{
	/**
	 * Test:
	 * * Empty and Clear
	 * * reuse of mapping
	 * * assignment (reverse order)
	 * * lookup
	 */
	for (uint8_t rUniPar = minRUniPar; rUniPar <= maxRUniPar; ++rUniPar)
	{
		for (size_t componentSize = 3; componentSize < maxComponentSize; ++componentSize)
		{
			// find max n for this p
			size_t n = componentSize - 1;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, rUniPar, n, rUniPar);
			exit_if_fail (graph, "opmphmGraphNew");
			OpmphmInit opmphmInit;
			// dummy data
			opmphmInit.getName = test_opmphm_getName;
			opmphmInit.initSeed = (int32_t) 1;
			opmphmInit.data = (void **) 1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < rUniPar; ++r)
				{
					graph->edges[i].vertices[r] = (i + r) % componentSize;
				}
				graph->edges[i].order = n - 1 - i;
			}
			// save element last and create multiple edge
			uint32_t data[rUniPar];
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				data[r] = graph->edges[n - 1].vertices[r];
				graph->edges[n - 1].vertices[r] = graph->edges[0].vertices[r];
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &opmphmInit, n), "graph with cycles marked as acyclic");
			// restore last element
			for (uint8_t r = 0; r < rUniPar; ++r)
			{
				graph->edges[n - 1].vertices[r] = data[r];
			}
			succeed_if (!opmphmMapping (opmphm, graph, &opmphmInit, n), "acyclic graph marked as cyclic");
			exit_if_fail (opmphmAssignment (opmphm, graph, n, 0) == 0, "opmphmAssignment");
			for (size_t i = 0; i < n; ++i)
			{
				succeed_if (opmphmLookup (opmphm, n, graph->edges[i].vertices) == n - 1 - i, "lookup");
			}
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}


int main (int argc, char ** argv)
{
	printf ("OPMPHM      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basic ();
	test_opmphmGraphNew ();
	test_minComponentSize ();
	test_cyclicMultipleEdges ();
	test_cyclicCountUpEdges ();
	test_cyclicCountDownEdges ();
	test_acyclicDefaultOrder ();
	test_acyclicReverseOrder ();

	print_result ("test_opmphm");

	return nbError;
}
