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

int kdbLibInit(void)
{
	return 0;
}

kdbLibHandle kdbLibLoad(const char *module)
{
	char *name = malloc (strlen(module) + sizeof (".so") + 1);
	strcpy (name, module);
	strcat (name, ".so");

	kdbLibHandle ret = (kdbLibHandle) dlopen(name, RTLD_LAZY);

	free (name);
	return ret;
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
	return (kdbLibFunc) dlsym((void*) handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
	return dlclose((void*) handle);
}
