/**
 * @file
 *
 * @brief Loading modules under linux.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)

  The name of the module will be libname.
  A .so will be appended.
  This file will be loaded.

  The path were the plugins are located,
  e.g. /usr/src/elektra
  need to be added with LD_LIBRARY_PATH.

  The reason is that only LD_LIBRARY_PATH
  also loads libraries which are seen by
  symlink only. That feature is needed for
  libelektra-default.

  The buildsystem makes sure that dlfcn.h
  exists.
  */

#include <kdbconfig.h>
#include <kdbmacros.h>

#include <dlfcn.h>

#include <kdberrors.h>
#include <kdbmodule.h>

#include <stdlib.h>
#include <string.h>

typedef struct _Module Module;

struct _Module
{
	void * handle;
	union
	{
		elektraPluginFactory f;
		void * v;
	} symbol;
};

int elektraModulesInit (ElektraKeyset * modules, ElektraKey * error ELEKTRA_UNUSED)
{
	elektraKeysetAppendKey (modules, elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END));

	return 0;
}

elektraPluginFactory elektraModulesLoad (ElektraKeyset * modules, const char * name, ElektraKey * errorKey)
{
#ifdef _WIN32
	static const char elektraPluginPostfix[] = ".dll";
#else
	static const char elektraPluginPostfix[] = ".so";
#endif

	ElektraKey * moduleKey = elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (moduleKey, name);
	ElektraKey * lookup = elektraKeysetLookup (modules, moduleKey, 0);
	if (lookup)
	{
		Module * module = (Module *) elektraKeyValue (lookup);
		elektraKeyDel (moduleKey);
		return module->symbol.f;
	}

	char * moduleName = elektraMalloc (sizeof ("libelektra-") + strlen (name) + sizeof (elektraPluginPostfix) + 1);

	strcpy (moduleName, "libelektra-");
	strcat (moduleName, name);
	strcat (moduleName, elektraPluginPostfix);

	Module module;
	module.handle = dlopen (moduleName,
#if (DEBUG || ASAN)
#ifdef RTLD_NODELETE
				RTLD_NODELETE |
#endif
#endif
					RTLD_NOW);

	if (module.handle == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Dlopen failed. Could not load module %s. Reason: %s", moduleName, dlerror ());
		elektraKeyDel (moduleKey);
		elektraFree (moduleName);
		return 0;
	}

	module.symbol.v = dlsym (module.handle, "elektraPluginSymbol");
	if (module.symbol.v == NULL)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (errorKey, "Dlsym failed. Could not get pointer to factory for module: %s. Reason: %s",
					       moduleName, dlerror ());
		dlclose (module.handle);
		elektraKeyDel (moduleKey);
		elektraFree (moduleName);
		return 0;
	}

	elektraKeySetBinary (moduleKey, &module, sizeof (Module));
	elektraKeysetAppendKey (modules, moduleKey);
	elektraFree (moduleName);

	return module.symbol.f;
}

int elektraModulesClose (ElektraKeyset * modules, ElektraKey * errorKey)
{
	ElektraKey * root = elektraKeysetLookupByName (modules, "system:/elektra/modules", ELEKTRA_KDB_O_POP);
	ElektraKey * cur;
	ElektraKeyset * newModules = 0;
	int ret = 0;

	if (!root)
	{
		ELEKTRA_ADD_INTERFACE_WARNING (errorKey, "Could not find root key system:/elektra/modules");
		return -1;
	}

	while ((cur = elektraKeysetPop (modules)) != 0)
	{
		Module * module = (Module *) elektraKeyValue (cur);
		if (dlclose (module->handle) != 0)
		{
			if (ret != -1)
			{
				/* First failure, start saving handles where close did not work */
				newModules = elektraKeysetNew (0, ELEKTRA_KS_END);
				elektraKeysetAppendKey (newModules, root);
			}
			ret = -1;
			ELEKTRA_ADD_RESOURCE_WARNINGF (errorKey, "Could not close a module. Dlclose failed: %s", dlerror ());

			elektraKeysetAppendKey (newModules, cur);
		}
		else
		{
			elektraKeyDel (cur);
		}
	}

	/* Clear dlerror */
	dlerror ();

	if (ret == -1)
	{
		elektraKeysetAppend (modules, newModules);
		elektraKeysetDel (newModules);
	}
	else
	{
		elektraKeyDel (root);
	}

	return ret;
}
