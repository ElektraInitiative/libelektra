#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "kdbloader.h"

int kdbLibInit(void)
{
}

kdbLibHandle kdbLibLoad(const char *module)
{
	kdbLibHandle	handle;
	handle = dlopen(module);
	return handle;
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
	return (kdbLibFunc) dlsym(handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
	return dlclose(handle);
}
