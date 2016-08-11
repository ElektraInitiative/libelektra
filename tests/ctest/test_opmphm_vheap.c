/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

/**
 * Comparison function for a maximum int heap
 */
static int maxcomp (void * a, void * b)
{
	if (!a || !b) return 1;
	return (*((int *)a) > *((int *)b));
}

/**
 * Comparison function for a minimum int heap
 */
static int mincomp (void * a, void * b)
{
	if (!a || !b) return 1;
	return (*((int *)a) < *((int *)b));
}

static void test_errors ()
{
	succeed_if (!elektraVheapInit (mincomp, 0), "init 0 working");
	succeed_if (!elektraVheapInit (mincomp, -1), "init -1 working");
	succeed_if (!elektraVheapInit (NULL, 1), "init NULL cmp working");

	succeed_if (elektraVheapIsEmpty (NULL) == -1, "isEmpty NULL working");

	succeed_if (!elektraVheapClear (NULL), "clear NULL working");

	succeed_if (!elektraVheapRemove (NULL), "remove NULL working");
	Vheap * h = elektraVheapInit (mincomp, 4);
	exit_if_fail (h, "vheap init error");
	succeed_if (!elektraVheapRemove (NULL), "remove empty working");
	elektraVheapDel (h);

	succeed_if (!elektraVheapInsert (NULL, NULL), "insert NULL working");
}


static void test_empty ()
{
	Vheap * h = elektraVheapInit (mincomp, 4);
	exit_if_fail (h, "vheap init error");
	succeed_if (elektraVheapIsEmpty (h), "should be empty");
	elektraVheapInsert (h, NULL);
	succeed_if (!elektraVheapIsEmpty (h), "should not be empty");
	elektraVheapRemove (h);
	succeed_if (elektraVheapIsEmpty (h), "should be empty");
	elektraVheapDel (h);
}

static void test_data_max_ordered ()
{
	Vheap * h = elektraVheapInit (maxcomp, 100);
	exit_if_fail (h, "vheap init error");
	int data[100];
	for (int i = 0; i < 99; ++i)
	{
		data[i] = i;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	int max = 100;
	for (int i = 0; i < 99; ++i)
	{

		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d < max, "ascending error");
		max = d;
	}
	elektraVheapDel (h);

	h = elektraVheapInit (maxcomp, 100);
	exit_if_fail (h, "vheap init error");
	for (int i = 99; i >= 0; --i)
	{
		data[i] = i;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	max = 100;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d < max, "descending error");
		max = d;
	}
	elektraVheapDel (h);
}

static void test_data_min_ordered ()
{
	Vheap * h = elektraVheapInit (mincomp, 100);
	exit_if_fail (h, "vheap init error");
	int data[100];
	for (int i = 0; i < 99; ++i)
	{
		data[i] = i;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	int min = -1;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d > min, "ascending error");
		min = d;
	}
	elektraVheapDel (h);

	h = elektraVheapInit (mincomp, 100);
	exit_if_fail (h, "vheap init error");
	for (int i = 99; i >= 0; --i)
	{
		data[i] = i;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	min = -1;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d > min, "descending error");
		min = d;
	}
	elektraVheapDel (h);
}

static void test_grow_shrink ()
{
	int maxElem = 101;
	int data = 42;
	for (int minSize = 1; minSize <= 100; ++minSize)
	{
		int actualSize = minSize;
		Vheap * h = elektraVheapInit (mincomp, minSize);
		exit_if_fail (h, "vheap init error");
		for (int i = 1; i <= maxElem; ++i)
		{
			succeed_if (elektraVheapInsert (h, &data), " insert error");
			if (i > actualSize)
			{
				// grow
				actualSize *= 2;
			}
			succeed_if ((size_t)actualSize == h->size, "grow error");
		}
		for (int i = maxElem - 1; i >= 0; --i)
		{
			succeed_if (elektraVheapRemove (h), "remove error");
			if (actualSize > minSize && i <= actualSize >> 2)
			{
				// shrink
				actualSize /= 2;
			}
			succeed_if ((size_t)actualSize == h->size, "shrink error");
		}
		elektraVheapDel (h);
	}
}

static void test_data_max_mixed ()
{
	Vheap * h = elektraVheapInit (maxcomp, 100);
	exit_if_fail (h, "vheap init error");
	int data[100];
	for (int i = 0; i < 99; ++i)
	{
		data[i] = i % 10;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	int max = 100;
	for (int i = 0; i < 99; ++i)
	{

		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d < max || d == max, "ascending error");
		max = d;
	}
	elektraVheapDel (h);

	h = elektraVheapInit (maxcomp, 100);
	exit_if_fail (h, "vheap init error");
	for (int i = 99; i >= 0; --i)
	{
		data[i] = i % 10;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	max = 100;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d < max || d == max, "descending error");
		max = d;
	}
	elektraVheapDel (h);
}

static void test_data_min_mixed ()
{
	Vheap * h = elektraVheapInit (mincomp, 100);
	exit_if_fail (h, "vheap init error");
	int data[100];
	for (int i = 0; i < 99; ++i)
	{
		data[i] = i % 10;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	int min = -1;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d > min || d == min, "ascending error");
		min = d;
	}
	elektraVheapDel (h);

	h = elektraVheapInit (mincomp, 100);
	exit_if_fail (h, "vheap init error");
	for (int i = 99; i >= 0; --i)
	{
		data[i] = i % 10;
		succeed_if (elektraVheapInsert (h, &data[i]), "insert error");
	}
	min = -1;
	for (int i = 0; i < 99; ++i)
	{
		void * dv = elektraVheapRemove (h);
		succeed_if (dv, "remove error");
		int d = *(int *)dv;
		succeed_if (d > min || d == min, "descending error");
		min = d;
	}
	elektraVheapDel (h);
}

static void test_clear ()
{
	Vheap * h = elektraVheapInit (mincomp, 5);
	exit_if_fail (h, "vheap init error");
	succeed_if (elektraVheapClear (h), "clear fresh error");
	succeed_if (h->size == h->minSize, "minsize error");
	int data = 42;
	for (int i = 0; i < 11; ++i)
	{
		succeed_if (elektraVheapInsert (h, &data), "insert error");
	}
	size_t size = h->size;
	succeed_if (elektraVheapClear (h), "clear error");
	succeed_if (elektraVheapIsEmpty (h), "empty error");

	succeed_if (h->count == 0, "count error");
	succeed_if (h->size == size, "size error");

	// insert again
	succeed_if (elektraVheapInsert (h, &data), "insert error");
	succeed_if (elektraVheapInsert (h, &data), "insert error");

	succeed_if (h->size == size, "size error");

	// remove to trigger shrink
	succeed_if (elektraVheapRemove (h), "remove error");
	succeed_if (elektraVheapRemove (h), "remove error");

	succeed_if (h->size == h->minSize, "minsize error");

	elektraVheapDel (h);
}

int main (int argc, char ** argv)
{
	printf ("VHEAP       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_errors ();
	test_grow_shrink ();
	test_data_max_ordered ();
	test_data_min_ordered ();
	test_data_max_mixed ();
	test_data_min_mixed ();
	test_empty ();
	test_clear ();

	printf ("\ntest_opmphm_vheap RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
