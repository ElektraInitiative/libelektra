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
	// untested, thanks to William Ahern <william@wilbur.25thandClement.com>
	char path[256] = "/usr/lib/elektra/lib";
	char *name = malloc (strlen(path) + strlen(module) + sizeof (".so.0.0") + 1);
	strcpy (name, path);
	strcat (name, module);
	strcat (name, ".so.0.0");

	if (-1 == (fd = open(name, O_RDONLY))) return 0;

	snprintf(path, sizeof path, "/dev/fd/%d", fd);

	kdbLibHandle ret = (kdbLibHandle) dlopen(path, RTLD_LAZY);

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
