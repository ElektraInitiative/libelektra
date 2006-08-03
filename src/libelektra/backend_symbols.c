
/* $Id$ */

#include "kdbbackend.h"
#include "kdbLibLoader.h"

#ifdef ELEKTRA_STATIC

/* Make the compiler happy */
extern int libelektra_filesys_LTX_kdbBackendFactory;
#ifdef EXPERIMENTAL
extern int libelektra_ini_LTX_kdbBackendFactory;
#endif

kdblib_symbol kdb_exported_syms[] =
{
	{"libelektra-filesys", NULL },
	{"kdbBackendFactory", &libelektra_filesys_LTX_kdbBackendFactory },
#ifdef EXPERIMENTAL
	{"libelektra-ini", NULL },
        {"kdbBackendFactory", &libelektra_ini_LTX_kdbBackendFactory },
#endif
	{ NULL, NULL }
};


#endif

	{"libelektra-filesys", NULL},
	{"libelektra-fstab", NULL},
	{"libelektra-daemon", NULL},
	{"libelektra-berkeleydb", NULL},
	{"libelektratools", NULL},
	{"libelektra-filesys", NULL},
	{"libelektra-fstab", NULL},
	{"libelektra-daemon", NULL},
	{"libelektra-berkeleydb", NULL},
	{"libelektratools", NULL},
