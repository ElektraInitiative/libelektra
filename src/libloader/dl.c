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
