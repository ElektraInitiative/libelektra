/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include <elektra/core/errors.h>

#include <internal/exported_symbols.h>
#include <internal/macros/attributes.h>
#include <internal/pluginload/module.h>


int elektraModulesInit (KeySet * modules, Key * error ELEKTRA_UNUSED)
{
	ksAppendKey (modules, keyNew ("system:/elektra/modules", KEY_END));

	return 0;
}

static kdblib_symbol * elektraStaticLoad (const char * module)
{
	kdblib_symbol * current;
	current = kdb_exported_syms;
	while (current->name != NULL)
	{
		/* Skip symbols, we're searching for
		 * the module name */
		if (current->function == NULL && strcmp (current->name, module) == 0)
		{
			/* Go to the first symbol for this file */
			current++;
			return current;
		}

		current++;
	}

	return NULL;
}

static kdblib_symbol * elektraStaticSym (kdblib_symbol * handle, const char * symbol)
{
	kdblib_symbol * current;

	current = handle;
	/* For each symbol about this module */
	while (current->function != NULL)
	{
		if (strcmp (current->name, symbol) == 0) return current;

		current++;
	}

	return NULL;
}

elektraPluginFactory elektraModulesLoad (KeySet * modules, const char * name, Key * error)
{
	Key * moduleKey = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (moduleKey, name);
	Key * lookup = ksLookup (modules, moduleKey, 0);
	if (lookup)
	{
		kdblib_symbol * module = (kdblib_symbol *) keyValue (lookup);
		keyDel (moduleKey);
		return (elektraPluginFactory) module->function;
	}

	kdblib_symbol * handle = elektraStaticLoad (name);

	if (handle == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (error, "Did not find module: %s", name);
		keyDel (moduleKey);
		return 0;
	}

	kdblib_symbol * module = elektraStaticSym (handle, "elektraPluginSymbol");

	if (module == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (
			error, "Could not get pointer to factory, static sym failed: no such symbol elektraPluginSymbol");
		return 0;
	}

	keySetBinary (moduleKey, module, sizeof (kdblib_symbol));
	ksAppendKey (modules, moduleKey);

	return (elektraPluginFactory) module->function;
}

int elektraModulesClose (KeySet * modules, Key * error ELEKTRA_UNUSED)
{
	ksClear (modules);
	return 0;
}
