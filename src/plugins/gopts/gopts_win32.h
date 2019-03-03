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
#include <windows.h>


#include <kdbhelper.h>

// TODO: error handling

static int loadArgs (char *** argvp)
{
	// TODO: use GetCommandLine() and do string splitting with quotes etc.
	return 0;
}

static char ** loadEnvp (void)
{
	// TODO: parse GetEnvironmentStrings()
	return NULL;
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
