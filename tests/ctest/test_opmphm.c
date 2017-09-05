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
static const char * test_opmphm_getString (void * data ELEKTRA_UNUSED)
{
	return "Test Key Name";
}

const size_t maxP = 1000;

static void test_cyclic (void)
{
	{
		/**
		 * Test multiple edges, there is at least one edge multiple times
		 */
		for (size_t p = 2; p < maxP; ++p)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / OPMPHMR_PARTITE) + 1 <= p; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			exit_if_fail (p == opmphm->p, "part size");
			OpmphmInit init;
			// dummy data
			init.getString = test_opmphm_getString;
			init.initSeed = (int32_t)1;
			init.data = (void **)1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
				{
					graph->edges[i].h[r] = i % p;
				}
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &init, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
	{
		/**
		 * Test cycles, fill up edge.h[] with all possible edges, starting from (0,...,0), no multiple edges.
		 */
		for (size_t p = 3; p < maxP; ++p)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / OPMPHMR_PARTITE) + 1 <= p; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			exit_if_fail (p == opmphm->p, "part size");
			OpmphmInit init;
			// dummy data
			init.getString = test_opmphm_getString;
			init.initSeed = (int32_t)1;
			init.data = (void **)1;
			// fill
			uint32_t data[OPMPHMR_PARTITE];
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				data[r] = 0;
			}
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
				{
					graph->edges[i].h[r] = data[r];
				}
				// count up data
				uint8_t r = OPMPHMR_PARTITE;
				do
				{
					--r;
					data[r] = (data[r] + 1) % p;
				} while (data[r] == 0);
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &init, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
	{
		/**
		 * Test cycles, fill up edge.h[] with all possible edges, starting from (p - 1,...,p - 1), no multiple edges.
		 */
		for (size_t p = 3; p < maxP; ++p)
		{
			// find max n for this p
			size_t n;
			for (n = 1; ((n + 1) / OPMPHMR_PARTITE) + 1 <= p; ++n)
				;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n, 1);
			exit_if_fail (graph, "opmphmGraphNew");
			exit_if_fail (p == opmphm->p, "part size");
			OpmphmInit init;
			// dummy data
			init.getString = test_opmphm_getString;
			init.initSeed = (int32_t)1;
			init.data = (void **)1;
			// fill
			int32_t data[OPMPHMR_PARTITE];
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				data[r] = p - 1;
			}
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
				{
					graph->edges[i].h[r] = data[r];
				}
				// count down data
				uint8_t r = OPMPHMR_PARTITE;
				do
				{
					--r;
					--data[r];
					if (data[r] < 0)
					{
						data[r] = p - 1;
					}
				} while (data[r] == (int32_t)p - 1);
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &init, n), "graph with cycles marked as acyclic");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
}

static void test_acyclic (void)
{
	{
		/**
		 * Test:
		 * * Empty and Clear
		 * * reuse of mapping
		 * * assignment (default order)
		 * * lookup
		 */
		for (size_t p = 3; p < maxP; ++p)
		{
			// find max n for this p
			size_t n = p - 1;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			succeed_if (opmphmIsEmpty (opmphm), "empty marked as non empty");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n, OPMPHMR_PARTITE);
			exit_if_fail (graph, "opmphmGraphNew");
			exit_if_fail (p == opmphm->p, "part size");
			OpmphmInit init;
			// dummy data
			init.getString = test_opmphm_getString;
			init.initSeed = (int32_t)1;
			init.data = (void **)1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
				{
					graph->edges[i].h[r] = (i + r) % p;
				}
			}
			// save element last and create multiple edge
			int32_t data[OPMPHMR_PARTITE];
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				data[r] = graph->edges[n - 1].h[r];
				graph->edges[n - 1].h[r] = graph->edges[0].h[r];
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &init, n), "graph with cycles marked as acyclic");
			// restore last element
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				graph->edges[n - 1].h[r] = data[r];
			}
			succeed_if (!opmphmMapping (opmphm, graph, &init, n), "acyclic graph marked as cyclic");
			exit_if_fail (opmphmAssignment (opmphm, graph, n, 1) == 0, "opmphmAssignment");
			succeed_if (!opmphmIsEmpty (opmphm), "non empty marked as empty");
			for (size_t i = 0; i < n; ++i)
			{
				succeed_if (opmphmLookup (opmphm, graph->edges[i].h) == i, "lookup");
			}
			opmphmClear (opmphm);
			succeed_if (opmphmIsEmpty (opmphm), "empty marked as non empty");
			// cleanup
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
		}
	}
	{
		/**
		 * Test:
		 * * Empty and Clear
		 * * reuse of mapping
		 * * assignment (reverse order)
		 * * lookup
		 */
		for (size_t p = 3; p < maxP; ++p)
		{
			// find max n for this p
			size_t n = p - 1;
			// prep
			Opmphm * opmphm = opmphmNew ();
			exit_if_fail (opmphm, "opmphmNew");
			succeed_if (opmphmIsEmpty (opmphm), "empty marked as non empty");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n, OPMPHMR_PARTITE);
			exit_if_fail (graph, "opmphmGraphNew");
			exit_if_fail (p == opmphm->p, "part size");
			OpmphmInit init;
			// dummy data
			init.getString = test_opmphm_getString;
			init.initSeed = (int32_t)1;
			init.data = (void **)1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
				{
					graph->edges[i].h[r] = (i + r) % p;
				}
				graph->edges[i].order = n - 1 - i;
			}
			// save element last and create multiple edge
			int32_t data[OPMPHMR_PARTITE];
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				data[r] = graph->edges[n - 1].h[r];
				graph->edges[n - 1].h[r] = graph->edges[0].h[r];
			}
			// check
			succeed_if (opmphmMapping (opmphm, graph, &init, n), "graph with cycles marked as acyclic");
			// restore last element
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				graph->edges[n - 1].h[r] = data[r];
			}
			succeed_if (!opmphmMapping (opmphm, graph, &init, n), "acyclic graph marked as cyclic");
			exit_if_fail (opmphmAssignment (opmphm, graph, n, 0) == 0, "opmphmAssignment");
			succeed_if (!opmphmIsEmpty (opmphm), "non empty marked as empty");
			for (size_t i = 0; i < n; ++i)
			{
				succeed_if (opmphmLookup (opmphm, graph->edges[i].h) == n - 1 - i, "lookup");
			}
			opmphmClear (opmphm);
			succeed_if (opmphmIsEmpty (opmphm), "empty marked as non empty");
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

	test_cyclic ();
	test_acyclic ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
