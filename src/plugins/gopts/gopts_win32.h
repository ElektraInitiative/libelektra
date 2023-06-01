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

#include <internal/utility/alloc.h>
static int loadArgs (char *** argvp)
{
	int argc;

	LPWSTR * args = CommandLineToArgvW (GetCommandLineW (), &argc);
	if (args == NULL)
	{
		return 0;
	}

	char ** argv = elektraMalloc ((argc + 1) * sizeof (char *));

	for (int i = 0; i < argc; ++i)
	{
		// TODO: error handling?

		int size = WideCharToMultiByte (CP_UTF8, 0, args[i], -1, NULL, 0, NULL, NULL);
		argv[i] = elektraMalloc (size * sizeof (char));

		WideCharToMultiByte (CP_UTF8, 0, args[i], -1, argv[i], size, NULL, NULL);
	}

	LocalFree (args);

	*argvp = argv;

	return argc;
}

static char ** loadEnvp (void)
{
	LPTCH env = GetEnvironmentStrings ();
	if (env == NULL)
	{
		return NULL;
	}

	char * e = env;
	size_t size = 0;
	int count = 0;
	while (e[0] != '\0' && (e = strchr (e, '\0')) != NULL)
	{
		++count;
		++e;
	}

	char ** envp = elektraMalloc ((count + 1) * sizeof (char *));

	e = env;
	envp[0] = env;
	int index = 0;
	char * next = NULL;
	while (e[0] != '\0' && (next = strchr (e, '\0')) != NULL)
	{
		envp[index] = e;
		++index;
		e = next + 1;
	}
	envp[index] = e;
	envp[index + 1] = NULL;

	return envp;
}

static void cleanupArgs (int argc, char ** argv)
{
	for (int i = 0; i < argc; ++i)
	{
		elektraFree (argv[i]);
	}

	elektraFree (argv);
}

static void cleanupEnvp (char ** envp)
{
	if (envp != NULL)
	{
		FreeEnvironmentStrings (envp[0]);
	}
}

#endif // ELEKTRA_GOPTS_SYSCTL_H
