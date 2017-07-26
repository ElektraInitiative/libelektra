/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
	for (int t = OPMPHMTUPLE - 1; t >= 0; --t)
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
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		result += order->h[t] * getPower (w, OPMPHMTUPLE - t - 1);
	}
	return result;
}

const size_t maxNsimple = 1000000;
const size_t maxNcomplex = 1000;

static void test_basic_functions (void)
{
	printf ("test_basic_functions\n");
	Opmphm * opmphm = opmphmNew ();
	exit_if_fail (opmphm, "malloc");
	succeed_if (opmphmIsEmpty (opmphm), "opmphm should be empty");
	opmphmDel (opmphm);
}

static void test_test_functions (void)
{
	printf ("test_test_functions\n");
	for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
	{
		for (size_t n = 0; n < getPower (w, OPMPHMTUPLE); ++n)
		{
			OpmphmOrder o;
			getOrderedPair (&o, w, n);
			succeed_if (getNumber (&o, w) == n, "relation between Ordered Pair and Integers wrong");
		}
	}
}

static void test_mapping_success (void)
{
	printf ("test_mapping_success\n");
	/* test w, always full. w^OPMPHMTUPLE elements in each run.
	 * order.h[] in ascending order
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			// prepare data
			size_t n = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * n);
			exit_if_fail (datap, "malloc");
			// prepare opmphm
			Opmphm * opmphm = opmphmNew ();
			OpmphmOrder * order = opmphmNewOrder (n, true);
			exit_if_fail (opmphm, "malloc");
			exit_if_fail (order, "malloc");
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.data = datap;
			init.initSeed = 1;
			init.minOrder = 0;
			init.maxOrder = n - 1;
			// fill
			if (order)
			{
				for (size_t i = 0; i < n; ++i)
				{
					getOrderedPair (&order[i], w, i);
				}
			}
			// build
			OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
			exit_if_fail (sortOrder, "malloc");
			int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
			exit_if_fail (ret >= 0, "malloc");
			// check
			succeed_if (!ret, "not duplicate data marked as duplicate");
			if (sortOrder)
			{
				for (size_t i = 0; i < n - 1; ++i)
				{
					succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
				}
			}
			// cleanup
			opmphmDel (opmphm);
			elektraFree (order);
			elektraFree (sortOrder);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full.
	 * order.h[] in ascending order
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			// prepare data
			size_t power = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * power);
			exit_if_fail (datap, "malloc");
			for (size_t n = 2; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare opmphm
				Opmphm * opmphm = opmphmNew ();
				OpmphmOrder * order = opmphmNewOrder (n, true);
				exit_if_fail (opmphm, "malloc");
				exit_if_fail (order, "malloc");
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.data = datap;
				init.initSeed = 1;
				init.minOrder = 0;
				init.maxOrder = n - 1;
				// fill
				if (order)
				{
					for (size_t i = 0; i < n; ++i)
					{
						getOrderedPair (&order[i], w, i);
					}
				}
				// build
				OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
				exit_if_fail (sortOrder, "malloc");
				int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
				exit_if_fail (ret >= 0, "malloc");
				// check
				succeed_if (!ret, "not duplicate data marked as duplicate");
				if (sortOrder)
				{
					for (size_t i = 0; i < n - 1; ++i)
					{
						succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w),
							    "sort incorrect");
					}
				}
				// cleanup
				opmphmDel (opmphm);
				elektraFree (order);
				elektraFree (sortOrder);
			}
			// cleanup
			elektraFree (datap);
		}
	}
	/* test w, always full. w^OPMPHMTUPLE elements in each run.
	 * order.h[] in descending order
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			// prepare data
			size_t n = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * n);
			exit_if_fail (datap, "malloc");
			// prepare opmphm
			Opmphm * opmphm = opmphmNew ();
			OpmphmOrder * order = opmphmNewOrder (n, true);
			exit_if_fail (opmphm, "malloc");
			exit_if_fail (order, "malloc");
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.data = datap;
			init.initSeed = 1;
			init.minOrder = 0;
			init.maxOrder = n - 1;
			// fill
			if (order)
			{
				for (size_t i = 0; i < n; ++i)
				{
					getOrderedPair (&order[i], w, n - 1 - i);
				}
			}
			// build
			OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
			exit_if_fail (sortOrder, "malloc");
			int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
			exit_if_fail (ret >= 0, "malloc");
			// check
			succeed_if (!ret, "not duplicate data marked as duplicate");
			if (sortOrder)
			{
				for (size_t i = 0; i < n - 1; ++i)
				{
					succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w), "sort incorrect");
				}
			}
			// cleanup
			opmphmDel (opmphm);
			elektraFree (order);
			elektraFree (sortOrder);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full.
	 * order.h[] in descending order
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			// prepare data
			size_t power = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * power);
			exit_if_fail (datap, "malloc");
			for (size_t n = 2; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare opmphm
				Opmphm * opmphm = opmphmNew ();
				OpmphmOrder * order = opmphmNewOrder (n, true);
				exit_if_fail (opmphm, "malloc");
				exit_if_fail (order, "malloc");
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.data = datap;
				init.initSeed = 1;
				init.minOrder = 0;
				init.maxOrder = n - 1;
				// fill
				if (order)
				{
					for (size_t i = 0; i < n; ++i)
					{
						getOrderedPair (&order[i], w, n - 1 - i);
					}
				}
				// build
				OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
				exit_if_fail (sortOrder, "malloc");
				int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
				exit_if_fail (ret >= 0, "malloc");
				// check
				succeed_if (!ret, "not duplicate data marked as duplicate");
				if (sortOrder)
				{
					for (size_t i = 0; i < n - 1; ++i)
					{
						succeed_if (getNumber (sortOrder[i], w) < getNumber (sortOrder[i + 1], w),
							    "sort incorrect");
					}
				}
				// cleanup
				opmphmDel (opmphm);
				elektraFree (order);
				elektraFree (sortOrder);
			}
			// cleanup
			elektraFree (datap);
		}
	}
}


static void test_mapping_fail (void)
{
	printf ("test_mapping_fail\n");
	/* test w, always full. w^OPMPHMTUPLE elements in each run.
	 * order.h[] in ascending order
	 * only 1 duplicate, should be detected during partition
	 */
	{
		opmphmRatio = 1;
		for (size_t w = 2; w < opmphmGetWidth (maxNsimple); ++w)
		{
			// prepare data
			size_t n = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * n);
			exit_if_fail (datap, "malloc");
			// prepare opmphm
			Opmphm * opmphm = opmphmNew ();
			OpmphmOrder * order = opmphmNewOrder (n, true);
			exit_if_fail (opmphm, "malloc");
			exit_if_fail (order, "malloc");
			OpmphmInit init;
			init.getString = test_opmphm_getString;
			init.data = datap;
			init.initSeed = 1;
			init.minOrder = 0;
			init.maxOrder = n - 1;
			// fill
			for (size_t i = 0; i < n; ++i)
			{
				getOrderedPair (&order[i], w, i);
			}
			// set error
			order[n * (3 / 4)].h[OPMPHMTUPLE / 2] = (order[n * (3 / 4)].h[OPMPHMTUPLE / 2] + 1) % w;
			// build
			OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
			exit_if_fail (sortOrder, "malloc");
			int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
			exit_if_fail (ret >= 0, "malloc");
			// check
			succeed_if (ret, "duplicate data marked as ok");
			// cleanup
			opmphmDel (opmphm);
			elektraFree (order);
			elektraFree (sortOrder);
			elektraFree (datap);
		}
	}
	/* test w, from 1 to full-1.
	 * order.h[] in ascending order
	 * use the last for a duplicate
	 */
	{
		for (size_t w = 2; w < opmphmGetWidth (maxNcomplex); ++w)
		{
			// prepare data
			size_t power = getPower (w, OPMPHMTUPLE);
			void * datap = elektraMalloc (sizeof (void *) * power);
			exit_if_fail (datap, "malloc");
			for (size_t n = 2; n < power; ++n)
			{
				opmphmRatio = (double)power / n;
				// prepare opmphm
				Opmphm * opmphm = opmphmNew ();
				OpmphmOrder * order = opmphmNewOrder (n, true);
				exit_if_fail (opmphm, "malloc");
				exit_if_fail (order, "malloc");
				OpmphmInit init;
				init.getString = test_opmphm_getString;
				init.data = datap;
				init.initSeed = 1;
				init.minOrder = 0;
				init.maxOrder = n - 1;
				// fill
				if (order)
				{
					for (size_t i = 0; i < n; ++i)
					{
						getOrderedPair (&order[i], w, i);
					}
					for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
					{
						order[n - 1].h[t] = order[0].h[t];
					}
				}
				// build
				OpmphmOrder ** sortOrder = opmphmInit (opmphm, &init, order, n);
				exit_if_fail (sortOrder, "malloc");
				int ret = opmphmMapping (opmphm, &init, order, sortOrder, n);
				exit_if_fail (ret >= 0, "malloc");
				// check
				succeed_if (ret, "duplicate data marked as ok");
				// cleanup
				opmphmDel (opmphm);
				elektraFree (order);
				elektraFree (sortOrder);
			}
			// cleanup
			elektraFree (datap);
		}
	}
}


int main (int argc, char ** argv)
{
	printf ("OPMPHM INIT      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_test_functions ();
	test_basic_functions ();
	test_mapping_success ();
	test_mapping_fail ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
