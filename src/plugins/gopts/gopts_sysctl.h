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

#include <sys/types.h> // has to be included before sys/sysctl.h

#include <string.h>
#include <sys/sysctl.h>
#include <unistd.h>

#include <internal/utility/alloc.h>
extern char ** environ;

static int loadArgs (char *** argvp)
{
	int mib[4];
	mib[0] = CTL_KERN;
	mib[1] = KERN_PROC;
	mib[2] = KERN_PROC_ARGS;
	mib[3] = getpid ();
	size_t size;
	if (sysctl (mib, 4, NULL, &size, NULL, 0) == -1)
	{
		return 0;
	}
	char * buf = elektraMalloc (size);
	if (sysctl (mib, 4, buf, &size, NULL, 0) == -1)
	{
		elektraFree (buf);
		return 0;
	}

	size_t pos = 0;
	int argc = 0;
	while (pos < size)
	{
		pos += strlen (buf + pos) + 1;
		++argc;
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

static void cleanupArgs (int argc ELEKTRA_UNUSED, char ** argv)
{
	elektraFree (argv[0]);
	elektraFree (argv);
}

static void cleanupEnvp (char ** envp ELEKTRA_UNUSED)
{
}

#endif // ELEKTRA_GOPTS_SYSCTL_H
