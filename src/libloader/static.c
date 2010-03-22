#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "kdbloader.h"
#include "exported_symbols.h"

int kdbLibInit(void)
{
	return 0;
}

kdbLibHandle kdbLibLoad(const char *module)
{
	kdblib_symbol	*current;
	current = kdb_exported_syms;
	while ( current->name != NULL ) {
		/* Skip symbols, we're searching for
		 * the module name */
		if ( current->function == NULL && strcmp(current->name, module) == 0 ) {
			/* Go to the first symbol for this file */
			current++;
			return current;
		}

		current++;
	}

	return NULL;
}

kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol)
{
	kdblib_symbol	*current;

	current = handle;
	/* For each symbol about this module */
	while ( current->function != NULL ) {
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
