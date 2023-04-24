/**
 * @file
 *
 * @brief Source for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./gopts.h"

#include <elektra/type/conversion.h>
#include <elektra/kdb/errors.h>
#include <elektra/opts.h>
#include <internal/utility/assert.h>
#include <internal/utility/old_helper.h>

static int loadArgs (char *** argvp);
static char ** loadEnvp (void);
static void cleanupArgs (int argc, char ** argv);
static void cleanupEnvp (char ** envp);

#include "./gopts_impl.c"

#if defined(ELEKTRA_GOPTS_PROCFS)
#include "./gopts_procfs.h"
#elif defined(ELEKTRA_GOPTS_OSX)
#include "./gopts_osx.h"
#elif defined(ELEKTRA_GOPTS_WIN32)
#include "./gopts_win32.h"
#elif defined(ELEKTRA_GOPTS_SYSCTL)
#include "./gopts_sysctl.h"
#else
#error "No implementation available"
#endif

int elektraGOptsGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/gopts"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/gopts", KEY_VALUE, "gopts plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports", KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports/get", KEY_FUNC, elektraGOptsGet, KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports/hook/gopts/get", KEY_FUNC, elektraGOptsGet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/gopts/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	char ** argv;
	int argc;
	char ** envp;
	Key * optsParent;
	bool cleanupArgData;
	bool cleanupEnv;
	bool freeArgs;
	bool freeEnv;

	KeySet * global = elektraPluginGetGlobalKeySet (handle);
	Key * globalParent = ksLookupByName (global, "system:/elektra/gopts/parent", 0);

	if (globalParent != NULL)
	{
		optsParent = keyNew (keyString (globalParent), KEY_END);

		Key * kArgc = ksLookupByName (global, "system:/elektra/gopts/argc", 0);
		Key * kArgv = ksLookupByName (global, "system:/elektra/gopts/argv", 0);
		Key * kEnvp = ksLookupByName (global, "system:/elektra/gopts/envp", 0);

		Key * kArgs = ksLookupByName (global, "system:/elektra/gopts/args", 0);
		Key * kEnv = ksLookupByName (global, "system:/elektra/gopts/env", 0);


		if ((kArgc == NULL) != (kArgv == NULL))
		{
			ELEKTRA_SET_INTERFACE_ERROR (parentKey, "Either set both argc and argv or neither (global keyset).");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (kArgc != NULL && kArgv != NULL)
		{
			keyGetBinary (kArgc, &argc, sizeof (int));
			keyGetBinary (kArgv, &argv, sizeof (char **));

			freeArgs = false;
			cleanupArgData = false;
		}
		else if (kArgs != NULL)
		{
			const char * argsString = keyValue (kArgs);
			size_t argsSize = keyGetValueSize (kArgs) - 1;
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
			keyGetBinary (kEnvp, &envp, sizeof (char **));
			freeEnv = false;
			cleanupEnv = false;
		}
		else if (kEnv != NULL)
		{
			const char * envString = keyValue (kEnv);
			size_t envSize = keyGetValueSize (kEnv) - 1;
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
		optsParent = keyNew (keyName (parentKey), KEY_END);
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

	KeySet * config = elektraPluginGetConfig (handle);

	const Key * offsetKey = ksLookupByName (config, "/offset", 0);
	const Key * usageKey = ksLookupByName (config, "/help/usage", 0);
	const Key * prefixKey = ksLookupByName (config, "/help/prefix", 0);

	kdb_long_long_t offset;
	if (offsetKey != NULL)
	{
		if (!elektraKeyToLongLong (offsetKey, &offset) || offset < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "config key offset must be a non-negative integer, not %s",
								keyString (offsetKey));
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
		keyCopyAllMeta (parentKey, optsParent);
		keyDel (optsParent);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * helpKey = keyNew ("proc:/elektra/gopts/help", KEY_VALUE, "0", KEY_END);
	keyCopyAllMeta (helpKey, optsParent);
	ksAppendKey (returned, helpKey);
	keyDel (optsParent);

	if (ret == 1)
	{
		keySetString (helpKey, "1");

		const char * usage = usageKey == NULL ? NULL : keyString (usageKey);
		const char * prefix = prefixKey == NULL ? NULL : keyString (prefixKey);

		char * message = elektraGetOptsHelpMessage (helpKey, usage, prefix);
		Key * messageKey = keyNew ("proc:/elektra/gopts/help/message", KEY_VALUE, message, KEY_END);
		elektraFree (message);
		ksAppendKey (returned, messageKey);
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
