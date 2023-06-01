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

#include <crt_externs.h>
#include <string.h>

extern char ** environ;

static int loadArgs (char *** argvp)
{
	const int * argcp = _NSGetArgc ();
	if (!argcp)
	{
		return 0;
	}

	char *** argvp1 = _NSGetArgv ();
	if (!argvp1)
	{
		return 0;
	}
	*argvp = *argvp1;

	return *argcp;
}

static char ** loadEnvp (void)
{
	return environ;
}

static void cleanupArgs (int argc ELEKTRA_UNUSED, char ** argv ELEKTRA_UNUSED)
{
}

static void cleanupEnvp (char ** envp ELEKTRA_UNUSED)
{
}

#endif // ELEKTRA_GOPTS_SYSCTL_H
