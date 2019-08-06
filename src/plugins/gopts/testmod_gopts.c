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

// version 6 and 7 of clang-format don't agree whether it is supposed to be *[] or * [] so disable it here
// TODO: re-enable clang-format once version 7 is used on build server
// clang-format off
#define ARGS(NAME, ...) ((const char *[]){ TESTAPP_PATH, NAME, __VA_ARGS__, NULL })
#define ENVP(LD_LIB_PATH, ...) ((const char *[]){ LD_LIB_PATH, __VA_ARGS__, NULL })

#define NO_ARGS(NAME) ((const char *[]){ TESTAPP_PATH, NAME, NULL })
#define NO_ENVP(LD_LIB_PATH) ((const char *[]){ LD_LIB_PATH, NULL })
// clang-format on

static void run_test (const char ** argv, const char ** envp)
{
	printf ("test %s\n", argv[1]);
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

	if (WIFSIGNALED (status))
	{
		printf ("child process was killed by signal: %s", strsignal (WTERMSIG (status)));
		exit (1);
	}

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

	char * ldLibPath = elektraFormat ("LD_LIBRARY_PATH=%s", getenv ("LD_LIBRARY_PATH"));

	run_test (NO_ARGS (TEST_EMPTY), NO_ENVP (ldLibPath));

	run_test (NO_ARGS (TEST_SINGLEOPT), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "-capple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "-capple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "-c", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "-c", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "--longopt=apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "--longopt=apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "--longopt", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "--longopt", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_SINGLEOPT, "noopt"), NO_ENVP (ldLibPath));

	run_test (NO_ARGS (TEST_SINGLEENV), NO_ENVP (ldLibPath));
	run_test (NO_ARGS (TEST_SINGLEENV), ENVP (ldLibPath, "ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_SINGLEENV), ENVP (ldLibPath, "OTHER_ENV_VAR=apple"));

	run_test (NO_ARGS (TEST_TWOOPT), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-capple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-capple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-c", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-c", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt=apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt=apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "noopt"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-bapple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-bapple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-b", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-b", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt2=apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt2=apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple", "morearg"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-bapple", "-capple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "-bapple", "morearg", "-c", "apple"), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_TWOOPT, "--longopt2", "apple", "--longopt", "apple"), NO_ENVP (ldLibPath));

	run_test (NO_ARGS (TEST_TWOENV), NO_ENVP (ldLibPath));
	run_test (NO_ARGS (TEST_TWOENV), ENVP (ldLibPath, "ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP (ldLibPath, "OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP (ldLibPath, "ENV_VAR=apple", "OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP (ldLibPath, "OTHER_OTHER_ENV_VAR=apple"));
	run_test (NO_ARGS (TEST_TWOENV), ENVP (ldLibPath, "ENV_VAR=apple", "OTHER_ENV_VAR=apple", "OTHER_OTHER_ENV_VAR=apple"));

	run_test (NO_ARGS (TEST_MIXED), NO_ENVP (ldLibPath));
	run_test (ARGS (TEST_MIXED, "-capple"), ENVP (ldLibPath, "ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "-c", "apple"), ENVP (ldLibPath, "OTHER_ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "--longopt=apple"),
		  ENVP (ldLibPath, "ENV_VAR=apple", "OTHER_ENV_VAR=apple", "OTHER_OTHER_ENV_VAR=apple"));
	run_test (ARGS (TEST_MIXED, "--longopt", "apple"), ENVP (ldLibPath, "OTHER_ENV_VAR=apple"));

	elektraFree (ldLibPath);

	print_result ("testmod_gopts");

	return nbError;
}
