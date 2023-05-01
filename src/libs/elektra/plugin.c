/**
 * @file
 *
 * @brief Interna of plugin functionality.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
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

#include <elektra/core/errors.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>
#include <internal/kdb/config.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
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

	// TODO [new_backend]: change elektraPluginFactory to return contract KeySet instead of half-initialized Plugin*
	elektraPluginFactory pluginFactory = 0;

	if (!name || name[0] == '\0')
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Not a valid name supplied for a plugin: name is null or empty");
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
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Not a valid name supplied for a plugin: name contained slashes only");
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
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not call function exported by ELEKTRA_PLUGIN_EXPORT: %s", name);
		goto err_clup;
	}

	/* init reference counting */
	handle->refcounter = 1;
	handle->config = config;
	handle->modules = modules; // to enable the modules keyset to be used within plugins as well
	config = 0;		   // for err_clup case

	/* let the plugin initialize itself */
	if (handle->kdbOpen)
	{
		if ((handle->kdbOpen (handle, errorKey)) == -1)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (
				errorKey,
				"Open of plugin returned unsuccessfully: %s. Reason contains plugin, see other warnings for details", name);
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
		if (rc == -1) ELEKTRA_ADD_RESOURCE_WARNING (errorKey, "Method 'kdbClose()' failed");
	}

	ksDel (handle->config);
	elektraFree (handle);

	return rc;
}


/**
 * Retrieves a function exported by a plugin.
 *
 * @param  plugin Plugin handle
 * @param  name   Function name. Must be a valid key name suffix. May not contain the sequence '..'
 * @return        Pointer to function. NULL if function not found or not enough memory available
 */
size_t elektraPluginGetFunction (Plugin * plugin, const char * name)
{
	ELEKTRA_NOT_NULL (plugin);
	ELEKTRA_NOT_NULL (name);

	if (strstr (name, "..") != NULL)
	{
		// The sequence ".." is contained in the name.
		// For security and stability purposes we do not allow that.
		ELEKTRA_LOG_WARNING ("Can't get function '%s' from plugin because '..' is not allowed in function name", name);
		return 0;
	}

	KeySet * exports = ksNew (0, KS_END);
	Key * pk = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	ksRewind (exports);
	keyAddBaseName (pk, "exports");
	keyAddName (pk, name);

	Key * keyFunction = ksLookup (exports, pk, 0);
	if (!keyFunction)
	{
		ELEKTRA_LOG_DEBUG ("function \"%s\" from plugin \"%s\" not found", name, plugin->name);
		ksDel (exports);
		keyDel (pk);
		return 0;
	}

	size_t * buffer;
	size_t bufferSize = keyGetValueSize (keyFunction);
	buffer = elektraMalloc (bufferSize);
	if (buffer)
	{
		int result = keyGetBinary (keyFunction, buffer, bufferSize);
		if (result == -1 || buffer == NULL)
		{
			ELEKTRA_LOG_WARNING ("could not get function \"%s\" from plugin \"%s\"", name, plugin->name);
			return 0;
		}
	}

	size_t func = *buffer;

	elektraFree (buffer);
	ksDel (exports);
	keyDel (pk);

	return func;
}
