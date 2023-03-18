/**
 * @file
 *
 * @brief Source for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_GOPTS_SYSCTL_H
#define ELEKTRA_GOPTS_SYSCTL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <internal/utility/old_helper.h>

extern char ** environ;

static int loadArgs (char *** argvp)
{
	FILE * cmdline;
	if (access ("/proc/self/cmdline", F_OK) != -1)
	{
		cmdline = fopen ("/proc/self/cmdline", "rb");
	}
	else if (access ("/proc/curproc/cmdline", F_OK) != -1)
	{
		cmdline = fopen ("/proc/curproc/cmdline", "rb");
	}
	else
	{
		return 0;
	}

	char * arg = NULL;
	size_t size = 0;
	int argc = 0;
	while (getdelim (&arg, &size, 0, cmdline) != -1)
	{
		++argc;
	}
	free (arg);
	rewind (cmdline);

	char ** argv = elektraMalloc ((argc + 1) * sizeof (char *));

	arg = NULL;
	int index = 0;
	while (getdelim (&arg, &size, 0, cmdline) != -1)
	{
		argv[index] = elektraStrDup (arg);
		++index;
	}
	free (arg);
	fclose (cmdline);

	argv[argc] = NULL;
	*argvp = argv;

	return argc;
}

static char ** loadEnvp (void)
{
	return environ;
}

static void cleanupArgs (int argc, char ** argv)
{
	for (int i = 0; i < argc; ++i)
	{
		elektraFree (argv[i]);
	}

	elektraFree (argv);
}

static void cleanupEnvp (char ** envp ELEKTRA_UNUSED)
{
}

#endif // ELEKTRA_GOPTS_SYSCTL_H
