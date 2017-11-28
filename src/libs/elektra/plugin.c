/**
 * @file
 *
 * @brief Interna of plugin functionality.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include <kdberrors.h>
#include <kdbinternal.h>
#include <kdbversion.h>


/**
 * @retval 1 and an allocated string of the pluginName if a new plugins should be created.
 * @retval 2 and an allocated string of the referenceName if an old plugin should be used
 * @retval 3 and both if a new plugin should be created and made available for later
 *         back referencing.
 * @retval -1 on error
 */
int elektraProcessPlugin (Key * cur, int * pluginNumber, char ** pluginName, char ** referenceName, Key * errorKey)
{
	const char * fullname = keyBaseName (cur);
	size_t fullsize = keyGetBaseNameSize (cur);

	if (fullname[0] != '#')
	{
		ELEKTRA_ADD_WARNING (18, errorKey, fullname);
		return -1;
	}
	if (fullname[1] < '0' || fullname[1] > '9')
	{
		ELEKTRA_ADD_WARNING (19, errorKey, fullname);
		return -1;
	}
	*pluginNumber = fullname[1] - '0';
	if (*pluginNumber > NR_OF_PLUGINS)
	{
		ELEKTRA_ADD_WARNING (20, errorKey, fullname);
		return -1;
	}

	if (fullname[2] == '#')
	{
		char prefixReferenceName[] = "system/elektra/plugins/";

		/* We have a back reference here */
		if (fullname[fullsize - 2] == '#')
		{
			const char * iter = &fullname[3];
			size_t pluginNameSize = 1; /* For null character */
			size_t referenceNameSize = 0;
			/* We will introduce a new plugin */
			while (*iter != '#')
			{
				++iter;
				++pluginNameSize;
			}

			*pluginName = elektraMalloc (pluginNameSize);
			strncpy (*pluginName, &fullname[3], pluginNameSize);
			(*pluginName)[pluginNameSize - 1] = 0;

			referenceNameSize = fullsize - pluginNameSize - 4;
			++iter; /* advance to one after hash */
			*referenceName = elektraMalloc (referenceNameSize + sizeof (prefixReferenceName));
			strncpy (*referenceName, prefixReferenceName, sizeof (prefixReferenceName));
			strncat (*referenceName, iter, referenceNameSize);
			(*referenceName)[referenceNameSize + sizeof (prefixReferenceName) - 2] = 0;

			return 3;
		}
		else
		{
			/* We reference back to a plugin */

			*referenceName = elektraMalloc (fullsize - 3 + sizeof (prefixReferenceName) - 1);
			strncpy (*referenceName, prefixReferenceName, sizeof (prefixReferenceName));
			strncat (*referenceName, &fullname[3], fullsize - 3);

			return 2;
		}
	}
	else
	{
		*pluginName = elektraMalloc (fullsize - 2); /* don't alloc for #n */
		strncpy (*pluginName, &fullname[2], fullsize - 2);

		return 1;
	}

	/* Should not be reached */
	return 0;
}

/**
 * Load a plugin.
 *
 * The array of plugins must be set to 0.
 * Its length is NR_OF_PLUGINS.
 *
 * systemConfig will only be used, not deleted.
 *
 * @param config the config with the information how the
 *        plugins should be put together
 * @param systemConfig the shared (system) config for the plugins.
 *        Every plugin additional get this config.
 *
 * @retval -1 on failure
 */
int elektraProcessPlugins (Plugin ** plugins, KeySet * modules, KeySet * referencePlugins, KeySet * config, KeySet * systemConfig,
			   Key * errorKey)
{
	Key * root;
	Key * cur;

	ksRewind (config);

	root = ksNext (config);

	while ((cur = ksNext (config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			char * pluginName = 0;
			char * referenceName = 0;
			int pluginNumber = 0;

			if (elektraProcessPlugin (cur, &pluginNumber, &pluginName, &referenceName, errorKey) == -1)
			{
				elektraFree (pluginName);
				elektraFree (referenceName);
				ksDel (config);
				return -1;
			}

			if (pluginName)
			{
				Key * key = keyDup (cur);
				keyAddBaseName (key, "config");
				KeySet * cutConfig = ksCut (config, key);
				keyDel (key);

				KeySet * pluginConfig = elektraRenameKeys (cutConfig, "user");
				ksDel (cutConfig);
				if (!pluginConfig) return -1;
				ksAppend (pluginConfig, systemConfig);
				ksRewind (pluginConfig); /* TODO: bug ksAppend invalidates cursor */

				/* case 1, we create a new plugin,
				   note that errorKey is not passed here, because it would set error information
				   but we only want a warning instead. */
				plugins[pluginNumber] = elektraPluginOpen (pluginName, modules, pluginConfig, errorKey);
				if (!plugins[pluginNumber])
				{
					ELEKTRA_ADD_WARNING (64, errorKey, pluginName);
					/* Loading plugin did not work */
					elektraFree (pluginName);
					elektraFree (referenceName);
					ksDel (config);
					return -1;
				}

				/* case 2, we label it for later use */
				if (referenceName)
					ksAppendKey (referencePlugins,
						     keyNew (referenceName, KEY_BINARY, KEY_SIZE, sizeof (plugins[pluginNumber]), KEY_VALUE,
							     &plugins[pluginNumber], KEY_END));
			}
			else
			{
				/* case 3, we use an existing plugin */
				Key * lookup = ksLookup (referencePlugins, keyNew (referenceName, KEY_END), KDB_O_DEL);
				if (!lookup)
				{
					ELEKTRA_ADD_WARNING (65, errorKey, referenceName);
					/* Getting a reference plugin at a previous stage did not work.
					Note that this check is necessary, because loading the plugin could
					fail for example at errorplugins and at a later point, for example
					at setplugins it is tried to refer to that.*/
					elektraFree (referenceName);
					ksDel (config);
					return -1;
				}
				plugins[pluginNumber] = *(Plugin **)keyValue (lookup);
				++plugins[pluginNumber]->refcounter;
			}
			elektraFree (pluginName);
			elektraFree (referenceName);
		}
		else
		{
			ELEKTRA_ADD_WARNING (21, errorKey, keyString (cur));
		}
	}

	ksDel (config);
	return 0;
}

/**
 * Opens a plugin.
 *
 * The config will be used as is. So be sure to transfer ownership
 * of the config to it, with e.g. ksDup().
 * elektraPluginClose() will delete the config.
 *
 * @return a pointer to a new created plugin or 0 on error
 */
Plugin * elektraPluginOpen (const char * name, KeySet * modules, KeySet * config, Key * errorKey)
{
	Plugin * handle = 0;
	const char * n;

	elektraPluginFactory pluginFactory = 0;

	if (!name || name[0] == '\0')
	{
		ELEKTRA_ADD_WARNING (39, errorKey, "name is null or empty");
		goto err_clup;
	}

	n = name;
	while (*n != '\0')
	{
		if (*n == '/')
			++n;
		else
			break;
	}

	if (*n == '\0')
	{
		ELEKTRA_ADD_WARNING (39, errorKey, "name contained slashes only");
		goto err_clup;
	}

	pluginFactory = elektraModulesLoad (modules, name, errorKey);
	if (pluginFactory == 0)
	{
		/* warning already set by elektraModulesLoad */
		goto err_clup;
	}

	handle = pluginFactory ();
	if (handle == 0)
	{
		ELEKTRA_ADD_WARNING (6, errorKey, name);
		goto err_clup;
	}

	/* init reference counting */
	handle->refcounter = 1;
	handle->config = config;
	config = 0; // for err_clup case

	/* let the plugin initialize itself */
	if (handle->kdbOpen)
	{
		if ((handle->kdbOpen (handle, errorKey)) == -1)
		{
			ELEKTRA_ADD_WARNING (11, errorKey, name);
			elektraPluginClose (handle, errorKey);
			goto err_clup;
		}
	}

	ELEKTRA_LOG_DEBUG ("Finished loading plugin %s", name);
	return handle;

err_clup:
	ELEKTRA_LOG ("Failed to load plugin %s\n", name);
	ksDel (config);
	return 0;
}

int elektraPluginClose (Plugin * handle, Key * errorKey)
{
	int rc = 0;

	if (!handle) return 0;

	--handle->refcounter;

	/* Check if we have the last reference on the plugin (unsigned!) */
	if (handle->refcounter > 0) return 0;

	if (handle->kdbClose)
	{
		rc = handle->kdbClose (handle, errorKey);
		if (rc == -1) ELEKTRA_ADD_WARNING (12, errorKey, "kdbClose() failed");
	}

	ksDel (handle->config);
	elektraFree (handle);

	return rc;
}

static int elektraMissingGet (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * error)
{
	ELEKTRA_SET_ERROR (62, error, keyName (error));
	return -1;
}

static int elektraMissingSet (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * error)
{
	ELEKTRA_SET_ERROR (63, error, keyName (error));
	return -1;
}


Plugin * elektraPluginMissing (void)
{
	Plugin * returned;

	returned = elektraCalloc (sizeof (struct _Plugin));
	if (!returned) return 0;

	returned->name = "missing";
	returned->kdbGet = elektraMissingGet;
	returned->kdbSet = elektraMissingSet;
	return returned;
}

static int elektraVersionGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * error ELEKTRA_UNUSED)
{
	KeySet * info = elektraVersionKeySet ();
	ksAppend (returned, info);
	ksDel (info);
	return 1;
}

static int elektraVersionSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * error)
{
	KeySet * info = elektraVersionKeySet ();
	ELEKTRA_SET_ERROR_READ_ONLY (info, returned, error);
	return 0;
}

Plugin * elektraPluginVersion (void)
{
	Plugin * returned;

	returned = elektraCalloc (sizeof (struct _Plugin));
	if (!returned) return 0;

	returned->name = "version";
	returned->kdbGet = elektraVersionGet;
	returned->kdbSet = elektraVersionSet;
	return returned;
}
