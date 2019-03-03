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

#include <string.h>
#include <sys/sysctl.h>
#include <sys/types.h>

#include <kdbhelper.h>

// TODO: error handling

extern char ** environ;

static int loadArgs (char *** argvp)
{
	int mib[4];
	mib[0] = CTL_KERN;
	mib[1] = KERN_PROC;
	mib[2] = KERN_PROC_ARGS;
	mib[3] = -1;
	size_t size;
	sysctl (mib, 4, NULL, &size, NULL, 0);
	char * buf = elektraMalloc (size);
	sysctl (mib, 4, buf, &size, NULL, 0);

	size_t pos = 0;
	int argc = 0;
	while (pos < size)
	{
		pos += strlen (buf + pos) + 1;
		argc++;
	}

	char ** argv = elektraMalloc (argc * sizeof (char *));
	argv[0] = buf;
	pos = strlen (buf) + 1;
	for (int i = 1; i < argc; ++i)
	{
		argv[i] = buf + pos;
		pos += strlen (buf + pos) + 1;
	}

	*argvp = argv;

	return argc;
}

static char ** loadEnvp (void)
{
	return environ;
}

static void cleanupArgs (int argc, char ** argv)
{
	elektraFree (argv[0]);
	elektraFree (argv);
}

static void cleanupEnvp (char ** envp ELEKTRA_UNUSED)
{
}

#endif // ELEKTRA_GOPTS_SYSCTL_H
