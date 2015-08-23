#include <benchmark.h>
#include <search.h>

//mode true=build false=search
void runBenchmark (bool mode, void (*runInLoop) (int, int, int, ENTRY *, int *));
void insert (int n, int k, int r, ENTRY * data, int * times);
void search (int n, int k, int r, ENTRY * data, int * times);

//Data helpers
KeySet * readKeySet (int size, int version);
ENTRY * prepareData (KeySet * ks);
void freeData (ENTRY * data, int size);

//output file
FILE * output;

//TODO KURT print machine name + kernel bla
//TODO KURT strange segfault with no input files
int main(int argc, char** argv)
{
	//clean up before benchmark
	hdestroy ();

	bool b_build = true;
	bool b_search = true;

	if (argc == 2)
	{
		if (strcmp(argv[1],"build") == 0)
			b_search = false;
		if (strcmp(argv[1],"search") == 0)
			b_build = false;
	}

	if (b_build)
		runBenchmark (true, insert);

	if (b_search)
		runBenchmark (false, search);

	return EXIT_SUCCESS;
}

/* provides the skeleton for the benchmark runs.
 */
void runBenchmark (bool mode, void (*runInLoop) (int, int, int, ENTRY *, int *))
{
	for (int v=1;v <= KEYSET_VERSIONS;++v)
	{
		for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
		{
			KeySet * ks = readKeySet (n,v);

			char filename_out [BUFFER_FILENAME];
			if (mode)
				filename_out[0]='b';
			else
				filename_out[0]='s';
			sprintf(&filename_out[1], "_%i_%i.bench",n,v);
			output = fopen (&filename_out[0], "w");
			if (output == NULL)
			{
				printf ("output file could not be opened\n");
				exit (EXIT_FAILURE);
			}
			fprintf (output, "KeySet size;bucket size;time\n");

			/* k is the bucket count and goes from n to 2n.
			 * The number of steps from n to 2n is calculated
			 * with a mapping form the interval starting with
			 * the MIN_KEYSET_SIZE  and ending at MAX_KEYSET_SIZE
			 * to the interval starting at MIN_BUCKET_STEP and ending
			 * at MAX_BUCKET_STEP.
			 * ex:
			 * MIN_KEYSET_SIZE= 10
			 * MAX_KEYSET_SIZE= 1000
			 * MIN_BUCKET_STEP = 3
			 * MAX_BUCKET_STEP = 30
			 * n = 10 has 3 steps
			 * n = 250 has 9 steps
			 * n = 500 has 16 steps
			 * n = 750 has 23 steps
			 * n = 1000 has 30 steps
			 */
			double diff_n = MAX_KEYSET_SIZE - MIN_KEYSET_SIZE;
			double diff_k = MAX_BUCKET_STEP - MIN_BUCKET_STEP;
			double ratio = diff_k/diff_n;
			int bucket_step_count = (int) (n*ratio) + MIN_BUCKET_STEP;
			if(bucket_step_count > MAX_BUCKET_STEP)
				bucket_step_count = MAX_BUCKET_STEP;
			int bucket_step = (int) n/bucket_step_count;
			// round up
			if (bucket_step * bucket_step_count != n)
				++bucket_step;

			for (int k = n; k < n*2;k+=bucket_step)
			{
				fprintf (output, "%i;%i",n,k);
				int times [REPEATS];

				for (int r = 0;r < REPEATS;++r)
				{
					ENTRY * data = prepareData (ks);

					//call for worker
					runInLoop (n,k,r,data,&times[0]);

					freeData (data,n);
				}

				fprintf (output, ";%i\n",median (times, REPEATS));
			}

			ksDel(ks);
			fclose (output);
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
		   fclose (output);
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
		   fclose (output);
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

		Key * validate = ep->data;
		if (strcmp (keyValue(validate),GENDATA_KEY_VALUE) != 0)
		{
			printf ("correctness error while search\n");
			fclose (output);
			exit (EXIT_FAILURE);
		}
	}

	// take the maximum for this run
	times[r] = max (keys_searched_for, n);

	//cleanup
	hdestroy ();
}

KeySet * readKeySet (int size, int version)
{
	KeySet * out = ksNew(size, KS_END);
	//plugin open
	KeySet * modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);
	KeySet *conf = ksNew (0, KS_END);
	Plugin * plugin = elektraPluginOpen("dump", modules, conf, 0);
	if(!plugin)
	{
		printf ("dump plugin could not be opened\n");
		exit (EXIT_FAILURE);
	}

	char filename_in [BUFFER_FILENAME];
	sprintf (&filename_in[0], "%i_%i.edf",size,version);

	Key * pkey = keyNew(0);
	keySetString (pkey, &filename_in[0]);

	plugin->kdbGet (plugin, out, pkey);
	//TODO check for errors
	keyDel (pkey);

	//plugin close
	elektraPluginClose (plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);

	return out;
}

/* This helper generates the ENTRYs for the hash map, for having a fresh
 * data to insert on each run.
 */
ENTRY * prepareData (KeySet * ks)
{
	ENTRY * data = (ENTRY *) malloc (ksGetSize(ks) * sizeof (ENTRY));
	if (data == NULL)
	{
		printf ("Malloc fail\n");
		fclose (output);
		exit (EXIT_FAILURE);
	}
	ksRewind (ks);
	Key * iter_key;
	int i = 0;
	while ((iter_key = ksNext (ks)) != 0)
	{
		const char * toCopy = keyName (iter_key);
		data[i].key = (char *) malloc (strlen(toCopy) + 1);
		if (data[i].key == NULL)
		{
			printf ("Malloc fail\n");
			fclose (output);
			exit (EXIT_FAILURE);
		}
		strcpy (data[i].key,toCopy);
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
