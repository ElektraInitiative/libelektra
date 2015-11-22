/**
 * @file
 *
 * @brief Loading modules under linux.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)

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

#include <dlfcn.h>

#include <kdbmodule.h>
#include <kdberrors.h>

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

int elektraModulesInit (KeySet *modules, Key * error ELEKTRA_UNUSED)
{
	ksAppendKey (modules, keyNew ("system/elektra/modules", KEY_END));

	return 0;
}

#ifdef _WIN32
const char elektraPluginPostfix [] = ".dll";
#else
const char elektraPluginPostfix [] = ".so";
#endif

elektraPluginFactory elektraModulesLoad (KeySet *modules, const char *name, Key *errorKey)
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

	char *moduleName = elektraMalloc (sizeof("libelektra-")
			+ strlen(name)
			+ sizeof (elektraPluginPostfix)
			+ 1);

	strcpy (moduleName, "libelektra-");
	strcat (moduleName, name);
	strcat (moduleName, elektraPluginPostfix);

	Module module;
	module.handle = dlopen(moduleName, RTLD_LAZY);

	if (module.handle == NULL)
	{
		ELEKTRA_ADD_WARNINGF(1, errorKey, "of module: %s, because: %s", moduleName, dlerror());
		keyDel (moduleKey);
		elektraFree(moduleName);
		return 0;
	}

	module.symbol.v = dlsym(module.handle, "elektraPluginSymbol");
	if (module.symbol.v == NULL)
	{
		ELEKTRA_ADD_WARNINGF(2, errorKey, "of module: %s, because: %s", moduleName,  dlerror());
		dlclose(module.handle);
		keyDel (moduleKey);
		elektraFree(moduleName);
		return 0;
	}

	keySetBinary (moduleKey, &module, sizeof (Module));
	ksAppendKey (modules, moduleKey);
	elektraFree(moduleName);

	return module.symbol.f;
}

int elektraModulesClose (KeySet *modules, Key *errorKey)
{
	Key *root = ksLookupByName (modules, "system/elektra/modules", KDB_O_POP);
	Key *cur;
	KeySet *newModules = 0;
	int ret = 0;

	if (!root)
	{
		ELEKTRA_ADD_WARNING(3, errorKey, "no key system/elektra/modules");
		return -1;
	}

	while ((cur = ksPop(modules)) != 0)
	{
		Module *module = (Module*)keyValue(cur);
		if (dlclose(module->handle) != 0)
		{
			if (ret != -1)
			{
				/* First failure, start saving handles where close did not work */
				newModules = ksNew(0, KS_END);
				ksAppendKey (newModules, root);
			}
			ret = -1;
			ELEKTRA_ADD_WARNING(4, errorKey, dlerror());

			ksAppendKey(newModules, cur);
		} else {
			keyDel (cur);
		}
	}

	/* Clear dlerror */
	dlerror();

	if (ret == -1)
	{
		ksAppend (modules, newModules);
		ksDel (newModules);
	}
	else keyDel (root);

	return ret;
}
