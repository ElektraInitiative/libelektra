/**Loading modules under linux.

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
  */

#include <dlfcn.h>

#include "kdbloader.h"

#include <stdlib.h>
#include <string.h>

typedef struct _Module Module;

struct _Module
{
	void *handle;
	union {
		elektraPluginFactory f;
		void *v;
	} symbol;
};

int elektraModulesInit (KeySet *modules, Key *error)
{
	ksAppendKey (modules, keyNew ("system/elektra/modules", KEY_END));

	return 0;
}

elektraPluginFactory elektraModulesLoad (KeySet *modules, const char *name, Key *error)
{
	Key *moduleKey = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (moduleKey, name);
	Key *lookup = ksLookup(modules, moduleKey, 0);
	if (lookup)
	{
		Module *module = (Module*)keyValue(lookup);
		keyDel (moduleKey);
		return module->symbol.f;
	}

	char *moduleName = malloc (strlen(name) + sizeof (".so") + 1);
	strcpy (moduleName, name);
	strcat (moduleName, ".so");

	Module module;
	module.handle = dlopen(moduleName, RTLD_LAZY);
	free (moduleName);

	if (module.handle == NULL)
	{
		if (error)
		{
			keySetMeta (error, "error", "description reason module");
			keySetMeta (error, "error/description", "could not load module, dlopen failed");
			keySetMeta (error, "error/reason", dlerror());
			keySetMeta (error, "error/module", "modules");
		}
		return 0;
	}

	module.symbol.v = dlsym(module.handle, "elektraPluginSymbol");
	if (module.symbol.v == NULL)
	{
		if (error)
		{
			keySetMeta (error, "error", "description reason module");
			keySetMeta (error, "error/description", "could not get pointer to factory, dlsym failed");
			keySetMeta (error, "error/reason", dlerror());
			keySetMeta (error, "error/module", "modules");
		}
		return 0;
	}

	keySetBinary (moduleKey, &module, sizeof (Module));
	ksAppendKey (modules, moduleKey);

	return module.symbol.f;
}

int elektraModulesClose (KeySet *modules, Key *error)
{
	Key *root = ksLookupByName (modules, "system/elektra/modules", KDB_O_POP);
	Key *cur;
	KeySet *newModules = 0;
	int ret = 0;

	if (!root)
	{
		if (error)
		{
			keySetMeta (error, "error", "description module");
			keySetMeta (error, "error/description", "could not find root key");
			keySetMeta (error, "error/module", "modules");
		}
		return -1;
	}

	while ((cur = ksPop(modules)) != 0)
	{
		Module *module = (Module*)keyValue(cur);
		if (dlclose(module->handle) == 0)
		{
			if (ret != -1)
			{
				newModules = ksNew(0);
				ksAppendKey (newModules, root);
			}
			ret = -1;
			keySetMeta (error, "error", "description module");
			keySetMeta (error, "error/description", "could not close a module");
			keySetMeta (error, "error/module", "modules");

			ksAppendKey(newModules, cur);
		} else {
			keyDel (cur);
		}
	}

	if (!ret == -1) ksAppend (modules, newModules);
	else keyDel (root);

	return ret;
}
