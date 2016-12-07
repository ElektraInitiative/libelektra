/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#define OPMPHM_TEST
#include "../../src/libs/elektra/opmphm.c"
#include <tests_internal.h>

static const char * test_opmphm_getString (void * data ELEKTRA_UNUSED)
{
	return "Test Key Name";
}

static void test_success ()
{
	/*
	 * o o o ...
	 * | | |
	 * o o o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 1; n < 1000; ++n)
		{
			size_t r = opmphmRatio * n;

			OPMPHMinit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			void * datap[n];
			init.data = datap;
			OPMPHM opmphm;
			Vertex vertices[2 * r];
			Edge edges[n];
			for (size_t i = 0; i < n; ++i)
			{
				edges[i].h[0] = 42;
				edges[i].h[1] = i;
				edges[i].h[2] = i + r;
			}
			succeed_if (!opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
			for (size_t i = 0; i < n; ++i)
			{
				succeed_if (vertices[i].firstEdge == (int)i, "firstEdge wrong i < r");
				succeed_if (vertices[i + r].firstEdge == (int)i, "firstEdge wrong i > r");
				succeed_if (edges[i].nextEdge[0] == -1, "nextEdge[0] wrong");
				succeed_if (edges[i].nextEdge[1] == -1, "nextEdge[0] wrong");
				succeed_if (vertices[i].degree == 1, "degree wrong i > r");
				succeed_if (vertices[i + r].degree == 1, "degree wrong i < r");
			}
		}
	}
	/*
	 * o o o ...
	 * |/ /
	 *  o   o o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 1; n < 100; ++n)
		{
			size_t r = opmphmRatio * n;
			for (size_t allTo = r; allTo < 2 * r; ++allTo)
			{
				OPMPHMinit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				void * datap[n];
				init.data = datap;
				OPMPHM opmphm;
				Vertex vertices[2 * r];
				Edge edges[n];
				for (size_t i = 0; i < n; ++i)
				{
					edges[i].h[0] = 42;
					edges[i].h[1] = i;
					edges[i].h[2] = allTo;
				}
				succeed_if (!opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
				for (size_t i = 0; i < n; ++i)
				{
					succeed_if (vertices[i].firstEdge == (int)i, "firstEdge wrong i < r");
					succeed_if (vertices[i].degree == 1, "degree wrong i < r");
					succeed_if (edges[i].nextEdge[0] == -1, "nextEdge[0] wrong");
					if (i == 0)
					{
						succeed_if (edges[i].nextEdge[1] == -1, "nextEdge[1] wrong");
					}
					else
					{
						succeed_if (edges[i].nextEdge[1] == (int)i - 1, "nextEdge[1] wrong");
					}
					if (i + r == allTo)
					{
						succeed_if (vertices[i + r].degree == n, "degree wrong i > r");
						succeed_if (vertices[i + r].firstEdge == (int)n - 1, "firstEdge wrong i > r");
					}
					else
					{
						succeed_if (vertices[i + r].degree == 0, "degree wrong i > r");
						succeed_if (vertices[i + r].firstEdge == -1, "firstEdge wrong i > r");
					}
				}
			}
		}
	}
	/*
	 *  o  o o ...
	 * |\ \
	 * o o o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 1; n < 100; ++n)
		{
			size_t r = opmphmRatio * n;
			for (size_t allTo = 0; allTo < r; ++allTo)
			{
				OPMPHMinit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				void * datap[n];
				init.data = datap;
				OPMPHM opmphm;
				Vertex vertices[2 * r];
				Edge edges[n];
				for (size_t i = 0; i < n; ++i)
				{
					edges[i].h[0] = 42;
					edges[i].h[1] = allTo;
					edges[i].h[2] = i + r;
				}
				succeed_if (!opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
				for (size_t i = 0; i < n; ++i)
				{
					succeed_if (vertices[i + r].firstEdge == (int)i, "firstEdge wrong i > r");
					succeed_if (vertices[i + r].degree == 1, "degree wrong i > r");
					succeed_if (edges[i].nextEdge[1] == -1, "nextEdge[1] wrong");
					if (i == 0)
					{
						succeed_if (edges[i].nextEdge[0] == -1, "nextEdge[0] wrong");
					}
					else
					{
						succeed_if (edges[i].nextEdge[0] == (int)i - 1, "nextEdge[0] wrong");
					}
					if (i == allTo)
					{
						succeed_if (vertices[i].degree == n, "degree wrong i < r");
						succeed_if (vertices[i].firstEdge == (int)n - 1, "firstEdge wrong i < r");
					}
					else
					{
						succeed_if (vertices[i].degree == 0, "degree wrong i < r");
						succeed_if (vertices[i].firstEdge == -1, "firstEdge wrong i < r");
					}
				}
			}
		}
	}
	/*
	 * o o o ...
	 * full connected
	 * o o o ...
	 *
	 * n = r * r
	 *
	 */
	{
		for (size_t r = 2; r < 100; ++r)
		{
			size_t n = r * r;
			opmphmRatio = 1 / (double)r;
			if ((size_t) (opmphmRatio * n) != r)
			{
				continue;
			}
			OPMPHMinit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			void * datap[n];
			init.data = datap;
			OPMPHM opmphm;
			Vertex vertices[2 * r];
			Edge edges[n];
			int k, u;
			u = -1;
			k = r;
			for (size_t i = 0; i < n; ++i, ++k)
			{
				edges[i].h[0] = 42;
				if (i % r == 0)
				{
					++u;
					k = r;
				}
				edges[i].h[1] = u;
				edges[i].h[2] = k;
			}
			succeed_if (!opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
			for (size_t i = 0; i < r; ++i)
			{
				succeed_if (vertices[i].degree == r, "degree wrong i < r");
				succeed_if (vertices[i + r].degree == r, "degree wrong i > r");
				succeed_if (vertices[i].firstEdge == (int)(r * (i + 1)) - 1, "firstEdge wrong i < r");
				succeed_if (vertices[i + r].firstEdge == (int)(n - r + i), "firstEdge wrong i > r");
			}
			for (size_t i = 0; i < n; ++i)
			{
				if (i % r == 0)
				{
					succeed_if (edges[i].nextEdge[0] == -1, "nextEdge[0] wrong");
				}
				else
				{
					succeed_if (edges[i].nextEdge[0] == (int)(i - 1), "nextEdge[0] wrong");
				}
				if ((int)(i - r) >= 0)
				{
					succeed_if (edges[i].nextEdge[1] == (int)(i - r), "nextEdge[1] wrong");
				}
				else
				{
					succeed_if (edges[i].nextEdge[1] == -1, "nextEdge[1] wrong");
				}
			}
		}
	}
}


static void test_fail ()
{
	/*
	 * o  o  o ...
	 * | ||
	 * o  o  o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 2; n < 1000; ++n)
		{
			size_t r = opmphmRatio * n;

			OPMPHMinit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			void * datap[n];
			init.data = datap;
			OPMPHM opmphm;
			Vertex vertices[2 * r];
			Edge edges[n];
			for (size_t i = 0; i < n; ++i)
			{
				edges[i].h[0] = 42;
				if (i == (size_t) (n / 2))
				{
					edges[i].h[1] = i - 1;
					edges[i].h[2] = i + r - 1;
				}
				else
				{
					edges[i].h[1] = i;
					edges[i].h[2] = i + r;
				}
			}
			succeed_if (opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
		}
	}
	/*
	 * o  o  o ...
	 * | // /
	 * ( o  ) o  o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 2; n < 100; ++n)
		{
			size_t r = opmphmRatio * n;
			for (size_t allTo = r; allTo < 2 * r; ++allTo)
			{
				OPMPHMinit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				void * datap[n];
				init.data = datap;
				OPMPHM opmphm;
				Vertex vertices[2 * r];
				Edge edges[n];
				for (size_t i = 0; i < n; ++i)
				{
					edges[i].h[0] = 42;
					if (i == (size_t) (n / 2))
					{
						edges[i].h[1] = i - 1;
						edges[i].h[2] = allTo;
					}
					else
					{
						edges[i].h[1] = i;
						edges[i].h[2] = allTo;
					}
				}
				succeed_if (opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
			}
		}
	}
	/*
	 * ( o  ) o o ...
	 * | \\ \
	 * o  o  o ...
	 */
	{
		opmphmRatio = 1.0;
		for (size_t n = 2; n < 100; ++n)
		{
			size_t r = opmphmRatio * n;
			for (size_t allTo = 0; allTo < r; ++allTo)
			{
				OPMPHMinit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				void * datap[n];
				init.data = datap;
				OPMPHM opmphm;
				Vertex vertices[2 * r];
				Edge edges[n];
				for (size_t i = 0; i < n; ++i)
				{
					edges[i].h[0] = 42;
					if (i == (size_t) (n / 2))
					{
						edges[i].h[1] = allTo;
						edges[i].h[2] = i - 1;
					}
					else
					{
						edges[i].h[1] = allTo;
						edges[i].h[2] = i;
					}
				}
				succeed_if (opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
			}
		}
	}
	/*
	 * o o o ...
	 * full connected, with at least 1 double edge
	 * o o o ...
	 *
	 * n = r * r
	 *
	 */
	{
		for (size_t r = 2; r < 100; ++r)
		{
			size_t n = r * r;
			opmphmRatio = 1 / (double)r;
			if ((size_t) (opmphmRatio * n) != r)
			{
				continue;
			}
			OPMPHMinit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			void * datap[n];
			init.data = datap;
			OPMPHM opmphm;
			Vertex vertices[2 * r];
			Edge edges[n];
			int k, u;
			u = -1;
			k = r;
			for (size_t i = 0; i < n; ++i, ++k)
			{
				edges[i].h[0] = 42;
				if (i % r == 0)
				{
					++u;
					k = r;
				}
				if (i == (size_t) (n / 2))
				{
					edges[i].h[1] = u - 1;
					edges[i].h[2] = k;
				}
				else
				{
					edges[i].h[1] = u;
					edges[i].h[2] = k;
				}
			}
			succeed_if (opmphmMapping (&opmphm, vertices, edges, &init, n), "mapping fail");
		}
	}
}


int main (int argc, char ** argv)
{
	printf ("OPMPHM MAPPING      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_success ();
	test_fail ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
