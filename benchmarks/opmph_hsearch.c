#include <opmph_benchmarks.h>
#include <search.h>

#define BUCKET_MIN_STEP_COUNT 3
#define BUCKET_MAX_STEP_COUNT 30


void runBenchmark (void (*runInLoop) (int, int, int, ENTRY *, int *));
void insert (int n, int k, int r, ENTRY * data, int * times);
void search (int n, int k, int r, ENTRY * data, int * times);

//Data helpers
ENTRY * prepareData (KeySet * ks);
void freeData (ENTRY * data, int size);

//other helpers
int median (int values[], int count);
int max (int values[], int count);
int searchNext (int keys_searched_for[], int size);


//TODO KURT print machine name + kernel bla
int main(int argc, char** argv)
{
	//clean up before benchmark
	hdestroy ();

	bool b_insert = true;
	bool b_search = true;

	if (argc == 2)
	{
		if (strcmp(argv[1],"insert") == 0)
			b_search = false;
		if (strcmp(argv[1],"search") == 0)
			b_insert = false;
	}

	if (b_insert)
		runBenchmark (insert);

	if (b_search)
		runBenchmark (search);

	return EXIT_SUCCESS;
}

/* provides the skeleton for the benchmark runs.
 */
void runBenchmark (void (*runInLoop) (int, int, int, ENTRY *, int *))
{
	printf ("KeySet size;bucket size;time\n");
	for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
	{
		/* k is the bucket count and goes from n to 2n.
		 * The step count is calculated with this function
		 * roundup((rounddown(n/min)-rounddown(n/max))/2)
		 */
		int temp1 = (int) (n/BUCKET_MIN_STEP_COUNT);
		int temp2 = (int) (n/BUCKET_MAX_STEP_COUNT);
		int bucket_step = ((int)( (temp1-temp2) / 2 )) + 1;

		for (int k = n; k <= n*2;k+=bucket_step)
		{
			printf ("%i;%i",n,k);
			int times[REPEATS];
			KeySet * ks = generateKeySet(n);

			for (int r = 0;r < REPEATS;++r)
			{
				ENTRY * data = prepareData(ks);

				//call for worker
				runInLoop (n,k,r,data,&times[0]);

				freeData (data,n);
			}
			ksDel(ks);
			printf (";%i\n",median (times, REPEATS));
		}
	}
}

/* The actual benchmark procedure for the insert, executed for each
 * setting.
 */
void insert (int n, int k, int r, ENTRY * data, int * times)
{
	ENTRY * ep;
	struct timeval start;
	struct timeval end;

	gettimeofday (&start, 0);
	//measure

	//create
	hcreate (k);
	//insert
	for (int i = 0;i < n;++i)
	{
		ep = hsearch (data[i], ENTER);
		if (ep == NULL) {
		   printf ("ENTER failed\n");
		   exit (EXIT_FAILURE);
	   }
	}

	gettimeofday (&end, 0);
	times[r] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
						(end.tv_usec - start.tv_usec);

	//cleanup
	hdestroy ();
}

/* The actual benchmark procedure for the search, executed for each
 * setting.
 */
void search (int n, int k, int r, ENTRY * data, int * times)
{
	ENTRY * ep;
	ENTRY e;

	struct timeval start;
	struct timeval end;
	//this array saves all the times to search each key
	int keys_searched_for[n];

	for (int i = 0;i < n;++i) keys_searched_for[i] = -1;

	//prepare hash map
	hcreate (k);
	for (int i = 0;i < n;++i)
	{
		ep = hsearch (data[i], ENTER);
		if (ep == NULL) {
		   printf ("ENTER failed\n");
		   exit (EXIT_FAILURE);
	   }
	}

	// search for each key randomly and save the time
	for(int i = 0;i < n;++i)
	{
		int search_for = searchNext (keys_searched_for, n);
		char * lookfor = data[search_for].key;

		e.key = lookfor;

		gettimeofday (&start, 0);
		//measure

		ep = hsearch(e, FIND);

		gettimeofday (&end, 0);
		keys_searched_for[search_for] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
							(end.tv_usec - start.tv_usec);

	}

	// take the maximum for this run
	times[r] = max (keys_searched_for, n);

	//cleanup
	hdestroy ();
}

/* This helper generates the ENTRYs for the hash map, for having a fresh
 * data to insert on each run.
 */
ENTRY * prepareData (KeySet * ks)
{
	ENTRY * data = malloc (ksGetSize(ks) * sizeof (ENTRY));
	if (data == NULL)
	{
		printf ("Malloc fail\n");
		exit (EXIT_FAILURE);
	}
	ksRewind (ks);
	Key * iter_key;
	int i = 0;
	while ((iter_key = ksNext (ks)) != 0)
	{
		const char * toCopy = keyName (iter_key);
		char * dest = malloc (strlen(toCopy) + 1);
		if (dest == NULL)
		{
			printf ("Malloc fail\n");
			exit (EXIT_FAILURE);
		}
		strcpy (dest,toCopy);
		data[i].key = dest;
		data[i].data = iter_key;
		++i;
	}
	return data;
}

void freeData(ENTRY * data, int size)
{
	for (int i = 0;i < size;++i)
		free (data[i].key);
	free(data);
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
	for(int i = 1;i < count;++i)
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
	//~ for(int i = 0;i < count;++i) printf("times[%i]=%i\n",i,values[i]);
	qsort (&values[0] ,count ,sizeof (int) , comp_int);
	return values[(int)count/2];
}
