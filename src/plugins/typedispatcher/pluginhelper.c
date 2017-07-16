/**
 * @file
 *
 * @brief Source for typedispatcher plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "typehelper.h"
#include <kdbprivate.h>
#include <stdio.h>


// checks if the pluign exports a validateKey function and return a pointer to it found
static ValidateFunction pluginGetValidateFunction (Plugin * plugin)
{
	ValidateFunction f = NULL;
	KeySet * exports = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (parentKey, plugin->name);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	int rc = plugin->kdbGet (plugin, exports, parentKey);
	fprintf (stderr, "kdbGet(%s, exports, %s) returned %d\n", plugin->name, keyName (parentKey), rc);
#else
	plugin->kdbGet (plugin, exports, parentKey);
#endif
	keyAddBaseName (parentKey, "exports");
	keyAddBaseName (parentKey, "validateKey");
	Key * validateKey = ksLookup (exports, parentKey, KDB_O_NONE);
	if (!validateKey)
	{
#ifdef DEVBUILD
		fprintf (stderr, "ksLookup(exports, %s, KDB_O_NONE) returned NULL\n", keyName (parentKey));
#endif
	}
	else
	{
		f = *(ValidateFunction *)keyValue (validateKey);
	}
	ksDel (exports);
	keyDel (parentKey);
	return f;
}


// load/initialize plugin and create and append PluginConfig to DispatchConfig
static Key * initPlugin (DispatchConfig * config, const char * checkMeta)
{
	Key * lookup = ksLookupByName (config->pluginMeta, checkMeta, KDB_O_NONE);
	if (!lookup) return NULL;
	const char * pluginName = keyString (lookup);
	Plugin * plugin = NULL;
	Key * errorKey = keyNew (0, KEY_END);
	plugin = elektraPluginOpen (pluginName, config->modules, ksNew (0, KS_END), errorKey);
	if (!plugin)
	{
#ifdef DEVBUILD
		fprintf (stderr, "elektraPluginOpen %s failed\n", pluginName);
		fprintf (stderr, "number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
		fprintf (stderr, "description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
		fprintf (stderr, "ingroup: : %s\n", keyString (keyGetMeta (errorKey, "error/ingroup")));
		fprintf (stderr, "module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
		fprintf (stderr, "at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")),
			 keyString (keyGetMeta (errorKey, "error/line")));
		fprintf (stderr, "reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
		fprintf (stderr, "mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
		fprintf (stderr, "configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
#endif
		keyDel (errorKey);
		return NULL;
	}
	keyDel (errorKey);

	PluginConfig * pc = NULL;
	pc = elektraCalloc (sizeof (PluginConfig));

	pc->plugin = plugin;
	pc->f = pluginGetValidateFunction (plugin);
	if (!(pc->f))
	{
#ifdef DEVBUILD
		fprintf (stderr, "ERROR: %s doesn't export validateKey\n", pluginName);
#endif
		elektraPluginClose (plugin, NULL);
		elektraFree (pc);
		return NULL;
	}
	else
	{
		Key * pluginKey = keyNew (pluginName, KEY_META_NAME, KEY_BINARY, KEY_SIZE, sizeof (PluginConfig), KEY_VALUE, &pc, KEY_END);
		ksAppendKey (config->plugins, pluginKey);
		return pluginKey;
	}
}


// returns pointer to validateKey function of plugin providing checkMeta
// and calls initPlugin to load plugin if needed
ValidateFunction getValidateFunction (DispatchConfig * config, const char * checkMeta)
{
	if (!config || !checkMeta) return NULL;

	const Key * plugin = ksLookupByName (config->plugins, checkMeta, KDB_O_NONE);
	if (!plugin)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "plugin providing %s not loaded\n", checkMeta);
#endif
		plugin = initPlugin (config, checkMeta);
	}
	else
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "plugin %s already loaded\n", keyString (plugin));
#endif
	}

	if (!plugin)
	{
#ifdef DEVBUILD
		fprintf (stderr, "ERROR: failed to load plugin providing %s\n", checkMeta);
#endif
		return NULL;
	}

	PluginConfig * pc = *(PluginConfig **)keyValue (plugin);
	if (!pc)
		return NULL;
	else
		return pc->f;
}
