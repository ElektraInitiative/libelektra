/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <internal/utility/old_helper.h>
#include <internal/macros/utils.h>

#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUFFER_SIZE 4096

pthread_barrier_t * bar;

void * writer (void * pV_data ELEKTRA_UNUSED)
{
	Key * parent = keyNew ("user:/test/race", KEY_END);
	KDB * h = kdbOpen (NULL, parent);
	char buffer[BUFFER_SIZE];
	unsigned long tid = (unsigned long) pthread_self ();
	int pid = getpid ();
	snprintf (buffer, BUFFER_SIZE - 1, "user:/test/race/keys/%d/%lu", pid, tid);
	KeySet * ks = ksNew (20, KS_END);

	int retg = kdbGet (h, ks, parent);
	ksAppendKey (ks, keyNew (buffer, KEY_VALUE, "a value", KEY_END));

	pthread_barrier_wait (bar);
	int rets = kdbSet (h, ks, parent);

	if (rets != -1)
	{
		int retg2 = kdbGet (h, ks, parent);
		printf ("I (%d/%lu) won the race! Got return values from first get %d,"
			" from set %d, from second get %d\n",
			pid, tid, retg, rets, retg2);
	}
	else
	{
		printf ("I (%d/%lu) lost the race! Got %d and from set %d\n", pid, tid, retg, rets);
	}

	ksDel (ks);
	kdbClose (h, parent);
	keyDel (parent);

	// pthread_exit (NULL);
	return 0;
}

int main (int argc, char ** argv)
{
	if (argc != 4)
	{
		printf ("Usage %s <procs> <threads> <barriers>\n", argv[0]);
		printf ("This program tests race condition in Elektra\n");
		printf ("If you set barriers procs*threads, all threads will\n");
		printf ("start kdbSet() at roughly the same time\n");
		return 1;
	}

	// on error (0) is safe
	int num_procs = atoi (argv[1]);
	int num_threads = atoi (argv[2]);
	int num_barriers = atoi (argv[3]);

	if (num_barriers > num_procs * num_threads)
	{
		return 1;
	}

	pthread_barrierattr_t attr;
	if (pthread_barrierattr_init (&attr) != 0)
	{
		return 2;
	}

	if (pthread_barrierattr_setpshared (&attr, PTHREAD_PROCESS_SHARED) != 0)
	{
		return 3;
	}

	char shm_name[] = "shm_name_elektra_test_race";
	shm_unlink (shm_name); // may fail

	int shm_fd = shm_open (shm_name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

	if (shm_fd == -1)
	{
		return 6;
	}

	if (ftruncate (shm_fd, sizeof (pthread_barrier_t)) != 0)
	{
		return 7;
	}

	bar = mmap (NULL, sizeof (pthread_barrier_t), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);

	if (bar == MAP_FAILED)
	{
		shm_unlink (shm_name);
		return 8;
	}

	if (pthread_barrier_init (bar, &attr, num_barriers) != 0)
	{
		return 10;
	}

	if (pthread_barrierattr_destroy (&attr) != 0)
	{
		return 11;
	}


	int i;
	for (i = 0; i < num_procs; i++)
	{
		int pid = fork ();

		if (pid == -1)
		{
			// fork not successful
			return 12;
		}
		else if (pid == 0)
		{
			// child
			pthread_t * pwriter = elektraMalloc (num_threads * sizeof (pthread_t));
			if (!pwriter) return 13;
			for (i = 0; i < num_threads; i++)
				if (pthread_create (&pwriter[i], NULL, writer, (void *) 0) != 0) return 14;
			for (i = 0; i < num_threads; i++)
				pthread_join (pwriter[i], NULL);
			elektraFree (pwriter);
			return 0;
		}
	}

	int status = 0;
	int sumexitstatus = 0;
	for (i = 0; i < num_procs; i++)
	{
		wait (&status);
		int exitstatus = WEXITSTATUS (status);

		if (exitstatus)
		{
			sumexitstatus = 100 + exitstatus;
		}
	}


	if (pthread_barrier_destroy (bar) != 0)
	{
		return 40;
	}

	if ((shm_unlink (shm_name)) != 0)
	{
		return 41;
	}

	printf ("Test run finished\n");
	return sumexitstatus;
}
