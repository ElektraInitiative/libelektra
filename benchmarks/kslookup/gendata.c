#include <benchmark.h>
#include <gendata.h>

int main (int argc ELEKTRA_UNUSED, char** argv ELEKTRA_UNUSED)
{
	initRand ();
	//plugin open
	KeySet * modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = elektraPluginOpen(EXPORT_PLUGIN, modules, conf, 0);
	if(!plugin)
	{
		fprintf (stderr, "dump plugin could not be opened\n");
		return EXIT_FAILURE;
	}

	Key * pkey = keyNew(0);
	char filename[BUFFER_FILENAME];

	for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
	{
		for (int v=1;v <= KEYSET_VERSIONS;++v)
		{
			KeySet * ks = generateKeySet (n);

			sprintf (&filename[0], "%i_%i.edf", n, v);

			keySetString (pkey, &filename[0]);

			plugin->kdbSet (plugin, ks, pkey);

			ksDel (ks);
		}
	}

	keyDel (pkey);

	//plugin close
	elektraPluginClose (plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);

	return EXIT_SUCCESS;
}

/* generates a KeySet for the given size. Filled with random data.
 */
KeySet * generateKeySet (size_t size)
{
	KeySet * ks = ksNew (size, KS_END);

	for (size_t i = 0;i < size;++i)
	{
		char * name = generateKeyName (ks);
		ksAppendKey (ks, keyNew (name, KEY_VALUE, GENDATA_KEY_VALUE, KEY_END));
		free (name);
	}

	return ks;
}

/* Generates a random String, not present as KeyName in
 * the given KeySet.
 */
char * generateKeyName (KeySet * checkIfUnique)
{
	bool unique;
	char * randomString;
	do
	{
		unique = true;
		int depth = genRand(MAX_DEPTH - MIN_DEPTH) + MIN_DEPTH;
		int indexOfSlash[depth + 1];
		//starting slash
		indexOfSlash[0]=0;
		for (int i = 1;i < depth + 1;++i)
		{
			int wordlength = genRand(MAX_WORDLENGTH - MIN_WORDLENGTH);
			wordlength += MIN_WORDLENGTH;
			//the last slash in the index array is the null termination
			indexOfSlash[i] = indexOfSlash[i - 1] + wordlength + 1;
		}
		randomString = (char *) malloc(indexOfSlash[depth]);
		if (randomString == NULL)
		{
			fprintf (stderr, "Malloc fail\n");
			exit (EXIT_FAILURE);
		}

		//fill-up
		int j = 0;
		for (int i = 0;i < indexOfSlash[depth] - 1;++i)
		{
			if (indexOfSlash[j] == i)
			{
				++j;
				randomString[i] = '/';
			}else
				randomString[i] = getRandomChar ();
		}
		randomString[indexOfSlash[depth] - 1] = '\0';

		//check unique
		Key * key_found = ksLookupByName(checkIfUnique, randomString, KDB_O_NONE);
		if (key_found != NULL)
		{
			unique = false;
		}
		if(!unique) free (randomString);
	}
	while (!unique);
	return randomString;
}

/* returns a random char out of the ALPHABET string.
 */

char getRandomChar (void)
{
	return ALPHABET[genRand (strlen (ALPHABET))];
}
