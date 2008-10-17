#include <benchmarks.h>

int main ()
{
	int i;
	for (i=0; i< NUM_THREAD; i++) if (pthread_create (&preader[i], NULL, reader, (void *) 0) != 0) exit (1);
	for (i=0; i< NUM_THREAD; i++) if (pthread_create (&pwriter[i], NULL, writer, (void *) 0) != 0) exit (1);

	for (i=0; i< NUM_THREAD; i++) pthread_join (preader[i],NULL);
	for (i=0; i< NUM_THREAD; i++) pthread_join (pwriter[i],NULL);
	
	statistics();
	remover(0);

	pthread_exit (NULL);
	return nbError;
}

void * reader (void * pV_data)
{
	int i;
	KDB *h = kdbOpen ();
	KeySet * cmp = create_keyset();
	Key * k = keyNew (ROOT_KEY, KEY_END);

	for (i=0; i<NR; i++)
	{
		KeySet * set = ksNew (0);
		kdbGet (h, set, k, 0);
		compare_keyset (set, cmp);
		ksDel (set);
	}
	

	keyDel (k);
	ksDel (cmp);
	kdbClose (h);
	pthread_exit (NULL);
	return (NULL);
}

void * writer (void * pV_data)
{
	int i;
	KDB *h = kdbOpen ();
	KeySet * set = create_keyset();
	Key * k = keyNew (ROOT_KEY, KEY_END);

	for (i=0; i< NR; i++)
	{
		kdbSet (h, set, k, 0);
	}

	keyDel (k);
	ksDel (set);
	kdbClose (h);
	pthread_exit (NULL);
	return (NULL);
}

void * remover (void * pV_data)
{
	int i;
	KDB *h = kdbOpen ();
	KeySet * set = create_keyset();
	Key * k = keyNew (ROOT_KEY, KEY_END);
	Key * j;
	ksRewind (set);
	// output_keyset (set,0);
	while ((j = ksNext(set)) != 0) keyRemove (j);
	ksSort (set); ksRewind (set);
	// output_keyset (set,0);

	for (i=0; i< 1; i++)
	{
		kdbSet (h, set, k, 0);
	}

	keyDel (k);
	ksDel (set);
	kdbClose (h);
	pthread_exit (NULL);
	return (NULL);
}

