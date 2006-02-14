
/* $Id$ */

#include "kdbbackend.h"
#include "kdbLibLoader.h"

#ifdef __STATIC

/* Make the compiler happy */
extern int libelektra_filesys_LTX_kdbBackendFactory;
/* extern int libelektra_ini_LTX_kdbBackendFactory; */

kdblib_symbol kdb_exported_syms[] =
{
	{"libelektra-filesys", NULL },
	{"kdbBackendFactory", &libelektra_filesys_LTX_kdbBackendFactory },
/*
	{"libelektra-ini", NULL },
        {"kdbBackendFactory", &libelektra_ini_LTX_kdbBackendFactory },
*/	
	{ NULL, NULL }
};


#endif

