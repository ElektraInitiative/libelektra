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

#include <elektra/utility/format.h>
#include <internal/config.h>

#include <tests_plugin.h>

#include <config.c>

#include <sys/types.h>
#include <sys/wait.h>

#include "./testdata.h"

#define ARGS(NAME, ...) ((const char *[]){ TESTAPP_NAME, NAME, __VA_ARGS__, NULL })
#define ENVP(LD_LIB_PATH, ...) ((const char *[]){ LD_LIB_PATH, __VA_ARGS__, NULL })

#define NO_ARGS(NAME) ((const char *[]){ TESTAPP_NAME, NAME, NULL })
#define NO_ENVP(LD_LIB_PATH) ((const char *[]){ LD_LIB_PATH, NULL })

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
		execve (bindir_file (TESTAPP_NAME), (char * const *) argv, (char * const *) envp);

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

void test_global (void)
{
	printf ("test global\n");

	Key * parentKey = keyNew ("/tests/gopts", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("gopts");

	int argc = 4;
	char ** argv = (char **) (char *[]){ "gopts-test", "-capple", "--longopt=banana", "raspberry", NULL };
	char ** envp = (char **) (char *[]){ "ENV_VAR=carrot", "OTHER_ENV_VAR=strawberry", NULL };

	plugin->global =
		ksNew (4, keyNew ("system:/elektra/gopts/parent", KEY_VALUE, keyName (parentKey), KEY_END),
		       keyNew ("system:/elektra/gopts/argc", KEY_BINARY, KEY_SIZE, sizeof (int), KEY_VALUE, &argc, KEY_END),
		       keyNew ("system:/elektra/gopts/argv", KEY_BINARY, KEY_SIZE, sizeof (char **), KEY_VALUE, &argv, KEY_END),
		       keyNew ("system:/elektra/gopts/envp", KEY_BINARY, KEY_SIZE, sizeof (char **), KEY_VALUE, &envp, KEY_END), KS_END);

	KeySet * ks = ksNew (5, keyNew ("spec:/tests/gopts/apple", KEY_META, "opt", "c", KEY_END),
			     keyNew ("spec:/tests/gopts/banana", KEY_META, "opt/long", "longopt", KEY_END),
			     keyNew ("spec:/tests/gopts/raspberry", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
			     keyNew ("spec:/tests/gopts/carrot", KEY_META, "env", "ENV_VAR", KEY_END),
			     keyNew ("spec:/tests/gopts/strawberry", KEY_META, "env", "OTHER_ENV_VAR", KEY_END), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	output_error (parentKey);

	succeed_if_same_string (keyString (ksLookupByName (ks, "/tests/gopts/apple", 0)), "apple");
	succeed_if_same_string (keyString (ksLookupByName (ks, "/tests/gopts/banana", 0)), "banana");
	succeed_if_same_string (keyString (ksLookupByName (ks, "/tests/gopts/raspberry", 0)), "raspberry");
	succeed_if_same_string (keyString (ksLookupByName (ks, "/tests/gopts/carrot", 0)), "carrot");
	succeed_if_same_string (keyString (ksLookupByName (ks, "/tests/gopts/strawberry", 0)), "strawberry");

	ksDel (plugin->global);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
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

	test_global ();

	print_result ("testmod_gopts");

	return nbError;
}
