#include <benchmark.h>

//mode true=build false=search
void runBenchmark (bool mode, int (*runInLoop) (int, int, KeySet *, int *));
int build (int n, int r, KeySet * data, int * times);
int search (int n, int r, KeySet * data, int * times);

//Data helpers
KeySet * readKeySet (int size, int version);

//TODO KURT print machine name + kernel bla
//TODO KURT strange segfault with no input files
int main(int argc, char** argv)
{
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
void runBenchmark (bool mode, int (*runInLoop) (int, int, KeySet *, int *))
{
	for (int v=1;v <= KEYSET_VERSIONS;++v)
	{
		char filename_out [BUFFER_FILENAME];
		char c_mode;
		if (mode)
			c_mode = 'b';
		else
			c_mode = 's';
		sprintf (&filename_out[0], "kslookup_%c_%i.bench",c_mode,v);
		FILE * output = fopen (&filename_out[0], "w");
		if (output == NULL)
		{
			printf ("output file could not be opened\n");
			exit (EXIT_FAILURE);
		}
		fprintf (output, "KeySet size;time\n");

		for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
		{
			KeySet * ks = readKeySet (n,v);
			if (ks == NULL)
			{
				fclose (output);
				exit (EXIT_FAILURE);
			}

			fprintf (output, "%i",n);
			int times [REPEATS];

			for (int r = 0;r < REPEATS;++r)
			{
				KeySet * data = ksDup (ks);

				//call for worker
				if(runInLoop (n,r,data,&times[0]) < 0)
				{
					fclose (output);
					ksDel (data);
					ksDel (ks);
					exit (EXIT_FAILURE);
				}

				ksDel (data);
			}

			fprintf (output, ";%i\n",median (times, REPEATS));

			ksDel(ks);
		}
		fclose (output);
	}
}

/* The actual benchmark procedure for the build, executed for each
 * setting.
 */
int build (int n ELEKTRA_UNUSED, int r, KeySet * data, int * times)
{
	struct timeval start;
	struct timeval end;

	gettimeofday (&start, 0);
	//measure

	//build
	//TODO KURT set build flag here
	//TODO KURT check for errors
	ksLookup (data, NULL, KDB_O_NONE);

	gettimeofday (&end, 0);
	times[r] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
						(end.tv_usec - start.tv_usec);

	return 1;
}

/* The actual benchmark procedure for the search, executed for each
 * setting.
 */
int search (int n, int r, KeySet * data, int * times)
{
	Key * keysearchfor = keyNew(0);
	Key * keyfound;
	struct timeval start;
	struct timeval end;
	//this array saves all the times to search each key
	int keys_searched_for[n];

	for (int i = 0;i < n;++i) keys_searched_for[i] = -1;

	//prepare hash map
	//TODO KURT set build flag here
	//TODO KURT check for errors
	ksLookup (data, NULL, KDB_O_NONE);

	// search for each key randomly and save the time
	for(int i = 0;i < n;++i)
	{
		int search_for = searchNext (keys_searched_for, n);
		keySetName (keysearchfor, keyName (data->array[search_for]));

		gettimeofday (&start, 0);
		//measure

		//TODO KURT set hash search flag
		keyfound = ksLookup(data, keysearchfor, KDB_O_NONE);

		gettimeofday (&end, 0);
		keys_searched_for[search_for] = (int) (end.tv_sec - start.tv_sec) * 1000000 +
							(end.tv_usec - start.tv_usec);

		if (keyfound == NULL)
		{
			printf ("not found while search\n");
			keyDel (keysearchfor);
			return -1;
		}
		if (strcmp (keyName(keyfound), keyName(keysearchfor)) != 0)
		{
			printf ("found wrong Key while search\n");
			keyDel (keysearchfor);
			return -1;
		}
		if (strcmp (keyValue(keyfound), GENDATA_KEY_VALUE) != 0)
		{
			printf ("wrong Key value while search\n");
			keyDel (keysearchfor);
			return -1;
		}
	}
	keyDel (keysearchfor);

	// take the maximum for this run
	times[r] = max (keys_searched_for, n);
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
		printf ("dump plugin could not be opened\n");
		return NULL;
	}

	char filename_in [BUFFER_FILENAME];
	sprintf (&filename_in[0], "%i_%i.edf",size,version);

	Key * pkey = keyNew(0);
	keySetString (pkey, &filename_in[0]);

	plugin->kdbGet (plugin, out, pkey);
	//TODO KURT check for errors
	keyDel (pkey);

	//plugin close
	elektraPluginClose (plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);

	return out;
}
