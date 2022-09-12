/**
 * @file
 *
 * @brief Source for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "gopts.h"

#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbmacros.h>
#include <kdbopts.h>

static int loadArgs (char *** argvp);
static char ** loadEnvp (void);
static void cleanupArgs (int argc, char ** argv);
static void cleanupEnvp (char ** envp);

#include "gopts_impl.c"

#if defined(ELEKTRA_GOPTS_PROCFS)
#include "gopts_procfs.h"
#elif defined(ELEKTRA_GOPTS_OSX)
#include "gopts_osx.h"
#elif defined(ELEKTRA_GOPTS_WIN32)
#include "gopts_win32.h"
#elif defined(ELEKTRA_GOPTS_SYSCTL)
#include "gopts_sysctl.h"
#else
#error "No implementation available"
#endif

int elektraGOptsGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/gopts"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/gopts", ELEKTRA_KEY_VALUE, "gopts plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/gopts/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/gopts/exports/get", ELEKTRA_KEY_FUNC, elektraGOptsGet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/gopts/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	char ** argv;
	int argc;
	char ** envp;
	ElektraKey * optsParent;
	bool cleanupArgData;
	bool cleanupEnv;
	bool freeArgs;
	bool freeEnv;

	ElektraKeyset * global = elektraPluginGetGlobalKeySet (handle);
	ElektraKey * globalParent = elektraKeysetLookupByName (global, "system:/elektra/gopts/parent", 0);

	if (globalParent != NULL)
	{
		optsParent = elektraKeyNew (elektraKeyString (globalParent), ELEKTRA_KEY_END);

		ElektraKey * kArgc = elektraKeysetLookupByName (global, "system:/elektra/gopts/argc", 0);
		ElektraKey * kArgv = elektraKeysetLookupByName (global, "system:/elektra/gopts/argv", 0);
		ElektraKey * kEnvp = elektraKeysetLookupByName (global, "system:/elektra/gopts/envp", 0);

		ElektraKey * kArgs = elektraKeysetLookupByName (global, "system:/elektra/gopts/args", 0);
		ElektraKey * kEnv = elektraKeysetLookupByName (global, "system:/elektra/gopts/env", 0);


		if ((kArgc == NULL) != (kArgv == NULL))
		{
			ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Either set both argc and argv or neither (global keyset).");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (kArgc != NULL && kArgv != NULL)
		{
			elektraKeyGetBinary (kArgc, &argc, sizeof (int));
			elektraKeyGetBinary (kArgv, &argv, sizeof (char **));

			freeArgs = false;
			cleanupArgData = false;
		}
		else if (kArgs != NULL)
		{
			const char * argsString = elektraKeyValue (kArgs);
			size_t argsSize = elektraKeyGetValueSize (kArgs) - 1;
			const char * argPtr = argsString;

			argc = 0;
			while (argPtr < argsString + argsSize)
			{
				++argc;
				argPtr += strlen (argPtr) + 1;
			}

			argPtr = argsString;
			argv = elektraMalloc (argc * sizeof (const char *));
			for (int i = 0; i < argc; i++)
			{
				argv[i] = (char *) argPtr;
				argPtr += strlen (argPtr) + 1;
			}

			freeArgs = true;
			cleanupArgData = false;
		}
		else
		{
			argv = NULL;
			argc = loadArgs (&argv);

			freeArgs = false;
			cleanupArgData = true;
		}

		if (kEnvp != NULL)
		{
			elektraKeyGetBinary (kEnvp, &envp, sizeof (char **));
			freeEnv = false;
			cleanupEnv = false;
		}
		else if (kEnv != NULL)
		{
			const char * envString = elektraKeyValue (kEnv);
			size_t envSize = elektraKeyGetValueSize (kEnv) - 1;
			const char * envPtr = envString;

			size_t envCount = 0;
			while (envPtr < envString + envSize)
			{
				++envCount;
				envPtr += strlen (envPtr) + 1;
			}

			envPtr = envString;
			envp = elektraMalloc ((envCount + 1) * sizeof (const char *));
			for (size_t i = 0; i < envCount; i++)
			{
				envp[i] = (char *) envPtr;
				envPtr += strlen (envPtr) + 1;
			}
			envp[envCount] = NULL;

			freeEnv = true;
			cleanupEnv = false;
		}
		else
		{
			envp = loadEnvp ();

			freeEnv = false;
			cleanupEnv = true;
		}
	}
	else
	{
		optsParent = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_END);
		argv = NULL;
		argc = loadArgs (&argv);
		envp = loadEnvp ();
		freeArgs = false;
		freeEnv = false;
		cleanupArgData = true;
		cleanupEnv = true;
	}

	if (argv == NULL || envp == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "could not load current process' arguments");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ElektraKeyset * config = elektraPluginGetConfig (handle);

	const ElektraKey * offsetKey = elektraKeysetLookupByName (config, "/offset", 0);
	const ElektraKey * usageKey = elektraKeysetLookupByName (config, "/help/usage", 0);
	const ElektraKey * prefixKey = elektraKeysetLookupByName (config, "/help/prefix", 0);

	kdb_long_long_t offset;
	if (offsetKey != NULL)
	{
		if (!elektraKeyToLongLong (offsetKey, &offset) || offset < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "config key offset must be a non-negative integer, not %s",
								elektraKeyString (offsetKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	else
	{
		offset = 0;
	}

	int ret = elektraGetOpts (returned, argc - offset, (const char **) argv + offset, (const char **) envp, optsParent);

	if (cleanupArgData) cleanupArgs (argc, argv);
	if (cleanupEnv) cleanupEnvp (envp);
	if (freeArgs) elektraFree (argv);
	if (freeEnv) elektraFree (envp);

	if (ret == -1)
	{
		elektraKeyCopyAllMeta (parentKey, optsParent);
		elektraKeyDel (optsParent);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ElektraKey * helpKey = elektraKeyNew ("proc:/elektra/gopts/help", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END);
	elektraKeyCopyAllMeta (helpKey, optsParent);
	elektraKeysetAppendKey (returned, helpKey);
	elektraKeyDel (optsParent);

	if (ret == 1)
	{
		elektraKeySetString (helpKey, "1");

		const char * usage = usageKey == NULL ? NULL : elektraKeyString (usageKey);
		const char * prefix = prefixKey == NULL ? NULL : elektraKeyString (prefixKey);

		char * message = elektraGetOptsHelpMessage (helpKey, usage, prefix);
		ElektraKey * messageKey = elektraKeyNew ("proc:/elektra/gopts/help/message", ELEKTRA_KEY_VALUE, message, ELEKTRA_KEY_END);
		elektraFree (message);
		elektraKeysetAppendKey (returned, messageKey);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("gopts",
		ELEKTRA_PLUGIN_GET,	&elektraGOptsGet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
