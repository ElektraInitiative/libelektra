/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_errors ()
{
	succeed_if (!elektraVstackInit (0), "init 0 working");
	succeed_if (!elektraVstackInit (-1), "init -1 working");

	succeed_if (!elektraVstackPush (NULL, NULL), "push NULL working");

	succeed_if (!elektraVstackPop (NULL), "pop NULL working");
	Vstack * s = elektraVstackInit (1);
	exit_if_fail (s, "vstack init error");
	succeed_if (!elektraVstackPop (s), "push empty working");
	elektraVstackDel (s);

	succeed_if (elektraVstackIsEmpty (NULL) == -1, "isEmpty NULL working");

	succeed_if (!elektraVstackClear (NULL), "clear NULL working");
}

static void test_empty ()
{
	Vstack * s = elektraVstackInit (5);
	exit_if_fail (s, "vstack init error");
	succeed_if (elektraVstackIsEmpty (s), "should be empty");
	for (int i = 0; i < 5; ++i)
	{
		elektraVstackPush (s, NULL);
	}
	succeed_if (!elektraVstackIsEmpty (s), "should not be empty");
	elektraVstackPop (s);
	succeed_if (!elektraVstackIsEmpty (s), "should not be empty");
	elektraVstackPop (s);
	succeed_if (!elektraVstackIsEmpty (s), "should not be empty");
	elektraVstackPop (s);
	succeed_if (!elektraVstackIsEmpty (s), "should not be empty");
	elektraVstackPop (s);
	succeed_if (!elektraVstackIsEmpty (s), "should not be empty");
	elektraVstackPop (s);
	succeed_if (elektraVstackIsEmpty (s), "should be empty");
	elektraVstackDel (s);
}

static void test_data ()
{
	Vstack * s = elektraVstackInit (10);
	exit_if_fail (s, "vstack init error");

	char * data0 = { "TESTEST" };
	int data1 = 99;
	void * data2 = (void *)6767;
	char data3 = 'k';
	int * data4 = &data1;

	elektraVstackPush (s, data0);
	elektraVstackPush (s, &data1);
	elektraVstackPush (s, data2);
	elektraVstackPush (s, &data3);
	elektraVstackPush (s, data4);

	int * check4 = elektraVstackPop (s);
	char check3 = *((char *)elektraVstackPop (s));
	void * check2 = elektraVstackPop (s);
	int check1 = *((int *)elektraVstackPop (s));
	char * check0 = elektraVstackPop (s);

	succeed_if (data0 == check0, "data char *");
	succeed_if (!strcmp (data0, check0), "data char * eval");
	succeed_if (data1 == check1, "data int");
	succeed_if (data2 == check2, "data bool");
	succeed_if (data3 == check3, "data char");
	succeed_if (data4 == check4, "data int *");
	succeed_if (*data4 == *check4, "data int * eval");

	elektraVstackDel (s);
}

static void test_grow_shrink ()
{
	int maxElem = 101;
	int data = 42;
	for (int minSize = 1; minSize <= 100; ++minSize)
	{
		int size = minSize;
		Vstack * s = elektraVstackInit (minSize);
		exit_if_fail (s, "vstack init error");
		for (int i = 1; i <= maxElem; ++i)
		{
			succeed_if (elektraVstackPush (s, &data), "push error");
			if (i > size)
			{
				// grow
				size *= 2;
			}
			succeed_if (s->data[i - 1] == *(s->head - 1), "grow head error");
			succeed_if (s->size == (size_t)size, "size error");
		}
		for (int i = maxElem - 1; i >= 0; --i)
		{
			succeed_if (elektraVstackPop (s), "pop error");
			if (size > minSize && i <= size >> 2)
			{
				// shrink
				size /= 2;
			}
			succeed_if (s->data[i] == *s->head, "shrink head error");
			succeed_if (s->size == (size_t)size, "shrink size error");
		}
		elektraVstackDel (s);
	}
}

static void test_clear ()
{
	Vstack * s = elektraVstackInit (5);
	exit_if_fail (s, "vstack init error");
	succeed_if (elektraVstackClear (s), "clear fresh error");
	succeed_if (s->size == s->minSize, "minsize error");
	int data = 42;
	for (int i = 0; i < 11; ++i)
	{
		succeed_if (elektraVstackPush (s, &data), "push error");
	}
	size_t size = s->size;
	succeed_if (elektraVstackClear (s), "clear error");
	succeed_if (elektraVstackIsEmpty (s), "empty error");

	succeed_if (s->head == s->data, "stackpoimter error");
	succeed_if (s->size == size, "size error");

	// push again
	succeed_if (elektraVstackPush (s, &data), "push error");
	succeed_if (elektraVstackPush (s, &data), "push error");

	succeed_if (s->size == size, "size error");

	// pop to trigger shrink
	succeed_if (elektraVstackPop (s), "pop error");
	succeed_if (elektraVstackPop (s), "pop error");

	succeed_if (s->size == s->minSize, "minsize error");

	elektraVstackDel (s);
}

int main (int argc, char ** argv)
{
	printf ("VSTACK       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_errors ();
	test_grow_shrink ();
	test_empty ();
	test_data ();
	test_clear ();

	printf ("\ntest_opmphm_vstack RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
