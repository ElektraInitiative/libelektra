/* ltdl */
#include <ltdl.h>
#include "kdbloader.h"

/* typedef lt_dlhandle kdbLibHandle; */

int kdbLibInit(void)
{
  int init_errors = 0;
  init_errors = lt_dlinit();
  if (init_errors)
    return init_errors;
  return lt_dladdsearchdir(BACKEND_LIBDIR);
}

kdbLibHandle kdbLibLoad(const char *module)
{
	kdbLibHandle	handle;
	handle = lt_dlopenext(module);
	return handle;
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
  return (kdbLibFunc) lt_dlsym(handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
  return lt_dlclose(handle);
}
