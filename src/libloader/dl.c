#include <dlfcn.h>

#include "kdbloader.h"

int kdbLibInit(void)
{
	return 0;
}

kdbLibHandle kdbLibLoad(const char *module)
{
	return (kdbLibHandle) dlopen(module, RTLD_LAZY);
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
	return (kdbLibFunc) dlsym((void*) handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
	return dlclose((void*) handle);
}
