#include <opmph_benchmarks.h>

char getRandomChar (void);
char * generateKeyName (KeySet * checkIfUnique);

/* generateKeySet generates a KeySet
 * for the given size. Filled with random data.
 */
KeySet * generateKeySet (size_t size)
{
	KeySet * ks = ksNew (size, KS_END);

	for (size_t i = 0;i < size;++i)
	{
		ksAppendKey(ks, keyNew(generateKeyName(ks),
					KEY_VALUE, "some data", KEY_END));
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
		int depth = ( genRand() % (MAX_DEPTH - MIN_DEPTH) ) + MIN_DEPTH;
		int indexOfSlash[depth + 1];
		//starting slash
		indexOfSlash[0]=0;
		for (int i = 1;i < depth + 1;++i)
		{
			int wordlength = genRand() % (MAX_WORDLENGTH - MIN_WORDLENGTH);
			wordlength += MIN_WORDLENGTH;
			//the last slash in the index array is the null termination
			indexOfSlash[i] = indexOfSlash[i - 1] + wordlength + 1;
		}
		randomString = (char *) malloc(indexOfSlash[depth]);
		if (randomString == NULL)
		{
			printf ("Malloc fail\n");
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
				randomString[i] = getRandomChar();
		}
		randomString[indexOfSlash[depth] - 1] = '\0';

		//check unique
		ksRewind (checkIfUnique);
		Key * iter_key;
		while ((iter_key = ksNext (checkIfUnique)) != 0)
		{
			if(strcmp(keyName(iter_key),randomString) == 0)
				unique = false;
		}
		if(!unique) free (randomString);
	}
	while (!unique);
	return randomString;
}

/* This function will be called in short intervals,
 * therefore some more work needs to be done to get
 * not always the same char.
 * But this function surely can be improved.
 */

char getRandomChar (void)
{
	unsigned int random = genRand() % 1000;
	unsigned int b = random;
	for (unsigned int i = 0;i < random;++i)
	{
		b-=i;
	}
	return ALPHABET[b % strlen(ALPHABET)];
}

unsigned int genRand (void)
{
	struct timeval time;
	gettimeofday (&time, 0);
	return (int) time.tv_sec * 1000000 + time.tv_usec;
}
