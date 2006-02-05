#include <stdio.h>
#include "config.h"
#include <ltdl.h>
#include "kdb.h"
#include "kdbbackend.h"
#include "kdbLibLoader.h"

#ifdef __STATIC
/* Static case */

extern kdblib_symbol kdb_exported_syms[];

kdbLibHandle kdbLibLoad(const char *module)
{
	kdblib_symbol	*current;
	kdbLibHandle handle;
		
	current = kdb_exported_syms;
	while ( current->name != NULL ) {
		/* Skip symbols, we're searching for
		 * the module name */
		fprintf(stderr, "search for %s found %s\n", module, current->name);
		if ( current->function == NULL && strcmp(current->name, module) == 0 ) {
			/* Go to the first symbol for this file */
			current++;
			fprintf(stderr, "FOUND !\n");
			return current;
		}

		current++;
	}
      
	return NULL;
}

void *kdbLibSym(kdbLibHandle handle, const char *symbol)
{
	kdblib_symbol	*current;

	fprintf(stderr, "kdbLibSym()\n");
	current = handle;
	/* For each symbol about this module */
	while ( current->function != NULL ) {
		fprintf(stderr, "sarch = %s, found = %s\n", symbol, current->name);
		if ( strcmp(current->name, symbol) == 0 )
			return current->function;

		current++;
	}
	
	return NULL;
}

int kdbLibClose(kdbLibHandle handle)
{
	return 0;
}

#else
#ifdef WIN32
/* Windows dynamic case */
dbLibHandle kdbLibLoad(const char *module)
{
}

void *kdbLibSym(kdbLibHandle handle, const char *symbol)
{
}

int kdbLibClose(kdbLibHandle handle)
{
}

const char *kdbLibError()
{

}
#else
/* Generic case using libltdl */
int kdbLibInit(void)
{
  return lt_dlinit();
}

kdbLibHandle kdbLibLoad(const char *module)
{
	kdbLibHandle	handle;

	handle = lt_dlopenext(module);
	if ( handle == NULL )
		fprintf(stderr, "kdbLibLoad : %s", lt_dlerror());

	return handle;
}

void *kdbLibSym(kdbLibHandle handle, const char *symbol)
{
  return lt_dlsym(handle, symbol);
}

int kdbLibClose(kdbLibHandle handle)
{
  return lt_dlclose(handle);
}

const char *kdbLibError()
{
  return lt_dlerror();
}

#endif
#endif
