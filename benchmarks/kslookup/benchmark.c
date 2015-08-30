#include <benchmark.h>

//TODO KURT rename or refactor
unsigned int genRand (void)
{
	struct timeval time;
	gettimeofday (&time, 0);
	return (int) time.tv_sec * 1000000 + time.tv_usec;
}

/* Returns an random and free index for the search procedure,
 * where for each key a search is made.
 */
int searchNext (int keys_searched_for[], int size)
{
	int free_fields = 0;
	//get free fields to generate the right random number
	for (int i = 0;i < size;++i)
	{
		if (keys_searched_for[i] == -1) ++free_fields;
	}
	int next = genRand() % free_fields;
	free_fields = 0;
	//map the generated random number to the free fields
	for (int i = 0;i < size;++i)
	{
		if (keys_searched_for[i] == -1)
		{
			if (free_fields == next) return i;
			++free_fields;
		}
	}
	//should not happen
	return 0;
}

int max (int values[], int count)
{
	int max = values[0];
	for (int i = 1;i < count;++i)
	{
		max = (values[i] > max) ? values[i] : max;
	}
	return max;
}

int comp_int (const void * a, const void * b)
{
	int _a = * (int *) a;
	int _b = * (int *) b;
	if(_a < _b) return -1;
	if(_a > _b) return 1;
	return 0;
}

int median (int values[], int count)
{
	qsort (&values[0] ,count ,sizeof (int) , comp_int);
	return values[(int)(count/2)];
}
