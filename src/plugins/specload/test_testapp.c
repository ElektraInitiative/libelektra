/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

#include "./testdata.h"

#include <config.c>

#include <unistd.h>

#define BACKUP_STDIN() int stdin_copy = dup (STDIN_FILENO);

#define RESTORE_STDIN()                                                                                                                    \
	if (dup2 (stdin_copy, STDIN_FILENO) == -1)                                                                                         \
	{                                                                                                                                  \
		yield_error ("Could not execute testapp");                                                                                 \
	}                                                                                                                                  \
	close (stdin_copy);

#define START_TESTAPP(...)                                                                                                                 \
	BACKUP_STDIN ();                                                                                                                   \
	startTestApp (bindir_file (TESTAPP_NAME), ((char * const[]){ TESTAPP_NAME, __VA_ARGS__, NULL }));

void startTestApp (const char * app, char * const argv[])
{
	pid_t pid;
	int fd[2];

	if (pipe (fd) != 0)
	{
		yield_error ("Could not execute testapp");
		return;
	}

	pid = fork ();

	if (pid == -1)
	{
		yield_error ("Could not execute testapp");
		return;
	}

	if (pid == 0)
	{ /* child */
		if (dup2 (fd[1], STDOUT_FILENO) == -1)
		{
			exit (EXIT_FAILURE);
		}

		close (fd[0]);
		close (fd[1]);

		execv (app, argv);

		exit (EXIT_FAILURE);
	}

	close (fd[1]);


	if (dup2 (fd[0], STDIN_FILENO) == -1)
	{
		yield_error ("Could not execute testapp");
		return;
	}

	close (fd[0]);
}

static bool check_binary_file (int fd, size_t expectedSize, const unsigned char * expectedData)
{
	FILE * file = fdopen (fd, "rb");

	int c;
	size_t cur = 0;
	while (cur < expectedSize && (c = fgetc (file)) != EOF)
	{
		if (c != expectedData[cur])
		{
			char buf[255];
			snprintf (buf, 255, "byte %zd differs", cur);
			yield_error (buf)
		}
		++cur;
	}

	while (fgetc (file) != EOF)
	{
		cur++;
	}

	fclose (file);

	if (cur == expectedSize)
	{
		return true;
	}

	char buf[255];
	snprintf (buf, 255, "actual size %zd differs from expected size %zd", cur, expectedSize);
	yield_error (buf);
	return false;
}

void test_default (void)
{
	START_TESTAPP ("--elektra-spec");

	succeed_if (check_binary_file (STDIN_FILENO, default_spec_expected_size, default_spec_expected), "output differs");

	RESTORE_STDIN ();
}

void test_noparent (void)
{
	START_TESTAPP ("spec", "noparent");

	succeed_if (check_binary_file (STDIN_FILENO, noparent_spec_expected_size, noparent_spec_expected), "output differs");

	RESTORE_STDIN ();
}

int main (int argc, char ** argv)
{
	printf ("SPECLOAD_TESTAPP     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_default ();
	test_noparent ();

	print_result ("test_testapp");

	return nbError;
}
