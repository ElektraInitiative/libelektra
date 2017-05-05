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

static const char * test_opmphm_getString (void * data ELEKTRA_UNUSED)
{
	return "Test Key Name";
}

static void getOrderedPair (OpmphmOrder * order, size_t w, size_t i)
{
	size_t result = i;
	for (unsigned int t = 0; t < OPMPHMNOEIOP; ++t)
	{
		size_t temp = result / w;
		size_t rest = result - temp * w;
		result = temp;
		order->h[t] = rest;
	}
}
static size_t getPower (size_t p, size_t q)
{
	size_t result = 1;
	for (size_t t = 0; t < q; ++t)
	{
		result *= p;
	}
	return result;
}

static size_t getNumber (OpmphmOrder * order, size_t w)
{
	size_t result = 0;
	for (int t = OPMPHMNOEIOP - 1; t >= 0; --t)
	{

		result += order->h[t] * getPower (w, t);
	}
	return result;
}

const size_t maxNsimple = 10000000;
const size_t maxNcomplex = 10000;

static void test_success ()
{
	// test test functions
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			for (size_t n = 0; n < getPower (w, OPMPHMNOEIOP); ++n)
			{
				OpmphmOrder o;
				getOrderedPair (&o, w, n);
				succeed_if (getNumber (&o, w) == n, "relation between Ordered Pair and Integers wrong");
			}
		}
	}
	/* test w, always full. w^OPMPHMNOEIOP elements in each run.
	 * ascending order
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			size_t n = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * n);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * n);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			// prepare
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			init.data = datap;
			Opmphm opmphm;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				getOrderedPair (&order[i], w, i);
			}
			// check
			OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
			succeed_if (sortOrder, "not duplicate data marked as duplicate");
			for (size_t i = 0; i < n - 1; ++i)
			{
				succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
			}

			elektraFree (sortOrder);
			elektraFree (order);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full.
	 * ascending order
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			size_t power = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * power);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * power);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			for (size_t n = 1; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				init.data = datap;
				Opmphm opmphm;
				// fill
				for (size_t i = 0; i < n; ++i)
				{
					getOrderedPair (&order[i], w, i);
				}
				// check
				OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
				succeed_if (sortOrder, "not duplicate data marked as duplicate");
				for (size_t i = 0; i < n - 1; ++i)
				{
					succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
				}
				elektraFree (sortOrder);
			}
			elektraFree (order);
			elektraFree (datap);
		}
	}
	/* test w, always full. w^OPMPHMNOEIOP elements in each run.
	 * descending order
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			size_t n = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * n);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * n);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			// prepare
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			init.data = datap;
			Opmphm opmphm;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				getOrderedPair (&order[i], w, n - 1 - i);
			}
			// check
			OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
			succeed_if (sortOrder, "not duplicate data marked as duplicate");
			for (size_t i = 0; i < n - 1; ++i)
			{
				succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
			}

			elektraFree (sortOrder);
			elektraFree (order);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full.
	 * descending order
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			size_t power = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * power);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * power);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			for (size_t n = 1; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				init.data = datap;
				Opmphm opmphm;
				// fill
				for (size_t i = 0; i < n; ++i)
				{
					getOrderedPair (&order[i], w, n - 1 - i);
				}
				// check
				OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
				succeed_if (sortOrder, "not duplicate data marked as duplicate");
				for (size_t i = 0; i < n - 1; ++i)
				{
					succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
				}
				elektraFree (sortOrder);
			}
			elektraFree (order);
			elektraFree (datap);
		}
	}
}


static void test_fail ()
{
	/* test w, always full. w^OPMPHMNOEIOP elements in each run.
	 * ascending order
	 * only 1 duplicate, should be detected during partition
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			size_t n = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * n);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * n);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			// prepare
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.seed = 0;
			init.data = datap;
			Opmphm opmphm;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				getOrderedPair (&order[i], w, i);
			}
			order[n * (3 / 4)].h[OPMPHMNOEIOP / 2] = (order[n * (3 / 4)].h[OPMPHMNOEIOP / 2] + 1) % w;
			// check
			OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
			//~ succeed_if (!sortOrder, "duplicate data marked as ok");

			elektraFree (sortOrder);
			elektraFree (order);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full-1.
	 * ascending order
	 * use the last for a duplicate
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			size_t power = getPower (w, OPMPHMNOEIOP);
			void * datap = elektraMalloc (sizeof (void *) * power);
			OpmphmOrder * order = elektraMalloc (sizeof (OpmphmOrder) * power);
			exit_if_fail (datap, "malloc");
			exit_if_fail (order, "malloc");
			for (size_t n = 2; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.seed = 0;
				init.data = datap;
				Opmphm opmphm;
				// fill
				for (size_t i = 0; i < n; ++i)
				{
					getOrderedPair (&order[i], w, i);
				}
				for (unsigned int t = 0; t < OPMPHMNOEIOP; ++t)
				{
					order[n - 1].h[t] = order[0].h[t];
				}
				// check
				OpmphmOrder ** sortOrder = opmphmInit (&opmphm, &init, order, n);
				succeed_if (!sortOrder, "duplicate data marked as ok");
				elektraFree (sortOrder);
			}
			elektraFree (order);
			elektraFree (datap);
		}
	}
}


int main (int argc, char ** argv)
{
	printf ("OPMPHM INIT      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_success ();
	test_fail ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
