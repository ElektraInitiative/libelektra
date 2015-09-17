#include <benchmark.h>
#include <search.h>
#include <sys/time.h>

//mode true=build false=search
void runBenchmark (bool mode, int (*runInLoop) (int, int, int, ENTRY *, int *));
int build (int n, int k, int r, ENTRY * data, int * times);
int search (int n, int k, int r, ENTRY * data, int * times);

//Data helpers
KeySet * readKeySet (int size, int version);
ENTRY * prepareData (KeySet * ks);
void freeData (ENTRY * data, int size);

//TODO KURT print machine name + kernel bla ? uname ?
int main(int argc, char** argv)
{
	initRand ();
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
		runBenchmark (true, build);

	if (b_search)
		runBenchmark (false, search);

	return EXIT_SUCCESS;
}

/* provides the skeleton for the benchmark runs.
 */
void runBenchmark (bool mode, int (*runInLoop) (int, int, int, ENTRY *, int *))
{
	for (int v=1;v <= KEYSET_VERSIONS;++v)
	{
		for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
		{
			KeySet * ks = readKeySet (n,v);

			char filename_out [BUFFER_FILENAME];
			char c_mode;
			if (mode)
				c_mode = 'b';
			else
				c_mode = 's';
			sprintf (&filename_out[0], "hsearch_%c_%i_%i.bench",c_mode,n,v);
			FILE * output = fopen (&filename_out[0], "w");
			if (output == NULL)
			{
				fprintf (stderr, "output file could not be opened\n");
				ksDel(ks);
				exit (EXIT_FAILURE);
			}
			fprintf (output, "KeySet size;bucket size;time\n");

			/* k is the bucket count and goes from n to 2n.
			 * The number of steps from n to 3n is calculated
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
			if (bucket_step * bucket_step_count != n)
				++bucket_step;

			for (int k = n; k < n*3;k+=bucket_step)
			{
				fprintf (output, "%i;%i",n,k);
				int times [REPEATS];

				for (int r = 0;r < REPEATS;++r)
				{
					ENTRY * data = prepareData (ks);
					if (data == NULL)
					{
						fclose (output);
						ksDel(ks);
						exit (EXIT_FAILURE);
					}

					//call for worker
					if( runInLoop (n,k,r,data,&times[0]) < 0)
					{
						fclose (output);
						ksDel(ks);
						freeData (data,n);
						exit (EXIT_FAILURE);
					}

					freeData (data,n);
				}

				fprintf (output, ";%i\n",median (times, REPEATS));
			}

			ksDel(ks);
			fclose (output);
		}
	}
}

/* The actual benchmark procedure for the build, executed for each
 * setting.
 */
int build (int n, int k, int r, ENTRY * data, int * times)
{
	ENTRY * ep;
	struct timeval start;
	struct timeval end;

	gettimeofday (&start, 0);
//MEASURE START

	//create
	hcreate (k);
	//insert
	for (int i = 0;i < n;++i)
	{
		ep = hsearch (data[i], ENTER);
		if (ep == NULL)
		{
			fprintf (stderr, "ENTER failed\n");
			return -1;
		}
	}
//MEASURE END
	gettimeofday (&end, 0);
	times[r] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
						(end.tv_usec - start.tv_usec);

	//cleanup
	hdestroy ();
	return 1;
}

/* The actual benchmark procedure for the search, executed for each
 * setting.
 */
int search (int n, int k, int r, ENTRY * data, int * times)
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
		if (ep == NULL)
		{
			fprintf (stderr, "ENTER failed\n");
			return -1;
		}
	}

	// search for each key randomly and save the time
	for(int i = 0;i < n;++i)
	{
		int search_for = searchNext (keys_searched_for, n);
		char * lookfor = data[search_for].key;

		e.key = lookfor;

		gettimeofday (&start, 0);
//MEASURE START

		ep = hsearch(e, FIND);
//MEASURE END
		gettimeofday (&end, 0);
		keys_searched_for[search_for] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
							(end.tv_usec - start.tv_usec);

		if (ep == NULL)
		{
			fprintf (stderr, "not found while search\n");
			return -1;
		}
		Key * validate = ep->data;
		if (strcmp (keyName(validate), lookfor) != 0)
		{
			fprintf (stderr, "found wrong Key while search\n");
			return -1;
		}
		if (strcmp (keyValue(validate), GENDATA_KEY_VALUE) != 0)
		{
			fprintf (stderr, "wrong Key value while search\n");
			return -1;
		}
	}

	// take the maximum for this run
	times[r] = max (keys_searched_for, n);

	//cleanup
	hdestroy ();
	return 1;
}

KeySet * readKeySet (int size, int version)
{
	KeySet * out = ksNew(size, KS_END);
	//plugin open
	KeySet * modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);
	KeySet *conf = ksNew (0, KS_END);
	Plugin * plugin = elektraPluginOpen(EXPORT_PLUGIN, modules, conf, 0);
	if(!plugin)
	{
		fprintf (stderr, "dump plugin could not be opened\n");
		exit (EXIT_FAILURE);
	}

	char filename_in [BUFFER_FILENAME];
	sprintf (&filename_in[0], "%i_%i.edf",size,version);

	Key * pkey = keyNew(0);
	keySetString (pkey, &filename_in[0]);

	plugin->kdbGet (plugin, out, pkey);

	const Key * ekey = keyGetMeta (pkey,"error/description");
	if (ekey != 0)
	{
		fprintf (stderr, "%s\n", (char *) keyString(ekey));
		exit (EXIT_FAILURE);
	}

	keyDel (pkey);

	//plugin close
	elektraPluginClose (plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);

	return out;
}

/* generates the ENTRYs for the hash map
 */
ENTRY * prepareData (KeySet * ks)
{
	ENTRY * data = (ENTRY *) malloc (ksGetSize(ks) * sizeof (ENTRY));
	if (data == NULL)
	{
		fprintf (stderr, "Malloc fail\n");
		return NULL;
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
			fprintf (stderr, "Malloc fail\n");
			for (int j=0;j < i;++j) free (data[j].key);
			free (data);
			return NULL;
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
