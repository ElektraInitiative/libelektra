/**
 * @file
 *
 * @brief Tests for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#include <config.c>

#include <sys/types.h>
#include <sys/wait.h>

#include "testdata.h"

#define ARGS(NAME, ...) ((const char *[]){ NAME, TESTAPP_PATH, __VA_ARGS__, NULL })
#define ENVP(...) ((const char *[]){ __VA_ARGS__, NULL })

#define NO_ARGS(NAME) ((const char *[]){ NAME, TESTAPP_PATH, NULL })
#define NO_ENVP ((const char *[]){ NULL })

static void run_test (const char ** argv, const char ** envp)
{
	pid_t pid;


	pid = fork ();

	if (pid == -1)
	{
		yield_error ("Could not execute testapp");
		return;
	}

	if (pid == 0)
	{
		/* child */
		execve (TESTAPP_PATH, (char * const *) argv, (char * const *) envp);

		exit (EXIT_FAILURE);
	}

	/* parent */
	int status;
	do
	{
		pid_t w = waitpid (pid, &status, 0);
		if (w == -1)
		{
			perror ("waitpid");
			yield_error ("waitpid");
		}
	} while (!WIFEXITED (status) && !WIFSIGNALED (status));

	exit_if_fail (!WIFSIGNALED (status), "child process was killed by signal");

	if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
	{
		nbError += WEXITSTATUS (status);
		yield_error ("child process test failed");
	}
}


int main (int argc, char ** argv)
{
	printf ("GOPTS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	run_test (NO_ARGS (TEST_EMPTY), NO_ENVP);

	run_test (NO_ARGS (TEST_SINGLEOPT), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "-capple"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "-capple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "-c", "apple"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "-c", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "--longopt=apple"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "--longopt=apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "--longopt", "apple"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "--longopt", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_SINGLEOPT, "noopt"), NO_ENVP);

	run_test (NO_ARGS (TEST_SINGLEENV), NO_ENVP);
	run_test (NO_ARGS (TEST_SINGLEENV), ENVP ("ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_SINGLEENV), ENVP ("OTHER_ENV_VAR=apple"));

	run_test (NO_ARGS (TEST_TWOOPT), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-capple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-capple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-c", "apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-c", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt=apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt=apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt", "apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "noopt"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-bapple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-bapple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-b", "apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-b", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt2=apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt2=apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple", "morearg"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-bapple", "-capple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "-bapple", "morearg", "-c", "apple"), NO_ENVP);
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple", "--longopt", "apple"), NO_ENVP);

	run_test (NO_ARGS (TEST_TWOENV), NO_ENVP);
	run_test (NO_ARGS (TEST_TWOENV), ENVP ("ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP ("OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP ("ENV_VAR=apple", "OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP ("OTHER_OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP ("ENV_VAR=apple", "OTHER_ENV_VAR=apple", "OTHER_OTHER_ENV_VAR=apple"));

	run_test (NO_ARGS (TEST_MIXED), NO_ENVP);
	run_test (ARGS (TEST_MIXED, "-capple"), ENVP ("ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "-c", "apple"), ENVP ("OTHER_ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "--longopt=apple"), ENVP ("ENV_VAR=apple", "OTHER_ENV_VAR=apple", "OTHER_OTHER_ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "--longopt", "apple"), ENVP ("OTHER_ENV_VAR=apple"));

	print_result ("testmod_gopts");

	return nbError;
}
