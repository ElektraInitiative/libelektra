#include <benchmark.h>

unsigned int seed = 0;

/* change rand function here, if needed.
 */
int elektraRandr (unsigned int * r_seed)
{
	return rand_r(r_seed);
}

/* change seed here!
 */
void initRand (void)
{
	seed = 3;
}

unsigned int genRand (int modul)
{
	int out = elektraRandr (&seed);
	return (out % modul);
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
	int next = genRand(free_fields);
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
