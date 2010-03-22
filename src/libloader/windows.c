/* Windows case, non static */

#include <windows.h>
#include "kdbloader.h"

/* typedef HMODULE kdbLibHandle; */

int kdbLibInit(void)
{
  return 0;
}

kdbLibHandle kdbLibLoad(const char *module)
{
  char *modulename = malloc((sizeof(char)*strlen(module))+sizeof(".dll"));
  kdbLibHandle handle;
  strcpy(modulename, module);
  strcat(modulename, ".dll");
  handle = LoadLibrary(modulename);
  free(modulename);
  return handle;
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
  return GetProcAddress(handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
  return FreeLibrary(handle);
}

