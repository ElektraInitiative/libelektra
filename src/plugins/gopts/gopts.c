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
#include <kdbconfig.h>
#include <kdberrors.h>
#include <kdbhelper.h>
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


int elektraGOptsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/gopts"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/gopts", KEY_VALUE, "gopts plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/gopts/exports", KEY_END),
			       keyNew ("system/elektra/modules/gopts/exports/get", KEY_FUNC, elektraGOptsGet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/gopts/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	char ** argv = NULL;
	int argc = loadArgs (&argv);
	char ** envp = loadEnvp ();

	if (argv == NULL || envp == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int ret = elektraGetOpts (returned, argc, (const char **) argv, (const char **) envp, parentKey);

	cleanupArgs (argc, argv);
	cleanupEnvp (envp);

	if (ret == -1)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if (ret == 1)
	{
		Key * helpKey = keyNew ("proc/elektra/gopts/help", KEY_VALUE, "1", KEY_END);
		keyCopyAllMeta (helpKey, parentKey);
		ksAppendKey (returned, helpKey);
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
