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

/**
 * Detects whether we are in help mode or not.
 * DOES NOT set 'proc:/elektra/gopts/help' for use with elektraGetOptsHelpMessage().
 *
 * @retval 1 if --help is part of argv
 * @retval 0 otherwise
 * @retval -1 on error (could not load argv)
 */
int elektraGOptsIsHelpMode (void)
{
	char ** argv = NULL;
	int argc = loadArgs (&argv);

	if (argv == NULL)
	{
		return -1;
	}

	for (int i = 0; i < argc; ++i)
	{
		if (strcmp (argv[i], "--help") == 0)
		{
			return 1;
		}
	}

	return 0;
}


int elektraGOptsGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/gopts"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/gopts", KEY_VALUE, "gopts plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports", KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports/get", KEY_FUNC, elektraGOptsGet, KEY_END),
			       keyNew ("system:/elektra/modules/gopts/exports/ishelpmode", KEY_FUNC, elektraGOptsIsHelpMode, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/gopts/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	char ** argv = NULL;
	int argc = loadArgs (&argv);
	char ** envp = loadEnvp ();

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

	int ret = elektraGetOpts (returned, argc - offset, (const char **) argv + offset, (const char **) envp, parentKey);

	cleanupArgs (argc, argv);
	cleanupEnvp (envp);

	if (ret == -1)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if (ret == 1)
	{
		Key * helpKey = keyNew ("proc:/elektra/gopts/help", KEY_VALUE, "1", KEY_END);
		keyCopyAllMeta (helpKey, parentKey);
		ksAppendKey (returned, helpKey);

		const char * usage = usageKey == NULL ? NULL : keyString (usageKey);
		const char * prefix = prefixKey == NULL ? NULL : keyString (prefixKey);

		char * message = elektraGetOptsHelpMessage (parentKey, usage, prefix);
		Key * messageKey = keyNew ("proc/elektra/gopts/help/message", KEY_VALUE, message, KEY_END);
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
