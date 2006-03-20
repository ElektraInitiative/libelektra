/* $Id$ */

#ifndef KDBLIBLOADER_H
#define KDBLIBLOADER_H

/* #include "kdbbackend.h" */

/* Struct which contain export symbols
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
	void *function;
} kdblib_symbol;


#ifdef __STATIC

#define kdbLibInit() 0

typedef kdblib_symbol* kdbLibHandle;


#else



#ifdef WIN32
/* Windows case, non static */
#include <windows.h>
#define kdbLibInit() 0
typedef HMODULE kdbLibHandle;
#else
/* Default case */
#include <ltdl.h>
int kdbLibInit(void);
typedef lt_dlhandle kdbLibHandle;
#endif
#endif

/* Functions */
kdbLibHandle kdbLibLoad(const char *backendName);
void *kdbLibSym(kdbLibHandle handle, const char *symbol);
int kdbLibClose(kdbLibHandle handle);
const char *kdbLibError(void);

#endif /* KDBLIBLOADER_H */
