/* $Id$ */

#ifndef KDBLIBLOADER_H
#define KDBLIBLOADER_H


#if 0
#ifdef ELEKTRA_STATIC

/* The static case
 *
 * Struct which contain export symbols
 *  Format :
 *  --------
 *
 *  filename, NULL
 *  symbol1, &func1,
 *  symbol2, &func2,
 *  filename2, NULL
 *  symbol3, &func3,
 *  symboln, &funcn,
 *  NULL, NULL
 */
typedef struct {
	const char *name;
	void (*function)(void);
} kdblib_symbol;
typedef kdblib_symbol* kdbLibHandle;

extern kdblib_symbol kdb_exported_syms[];

#else
# ifdef WIN32

/* Windows case, non static */
# include <windows.h>
typedef HMODULE kdbLibHandle;

# else

/* Default case */
#  include <ltdl.h>
typedef lt_dlhandle kdbLibHandle;

# endif /* Default case */
#endif /* WIN32 */
#endif /* 0 */

/* Opaque pointer to any libhandle
TODO: private, not linux only */
typedef void* kdbLibHandle;

/* General pointer to kdbLib Functions and pointer to kdbLibBackend function */
typedef void (*kdbLibFunc)(void);

/* Functions */
int kdbLibInit(void);
kdbLibHandle kdbLibLoad(const char *backendName);
kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol);
int kdbLibClose(kdbLibHandle handle);
const char *kdbLibError(void);

#endif /* KDBLIBLOADER_H */
