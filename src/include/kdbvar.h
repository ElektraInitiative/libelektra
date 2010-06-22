/***************************************************************************
      kdbvar.h  -  Configuration variables

                           -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

@DISCLAMER@

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef KDBVAR_H
#define KDBVAR_H

#ifndef KDB_DB_SYSTEM
/**Below this directory the system configuration (system/) will be searched.*/
#define KDB_DB_SYSTEM            "@KDB_DB_SYSTEM@"
#endif

#ifndef KDB_DB_USER
/**This directory will be concatenated with a prefix which will be searched
 * at runtime inside kdbbGetFullFilename().
 *@see kdbbGetFullFilename
 */
#define KDB_DB_USER              ".kdb"
#endif

#ifndef KDB_DB_HOME
/**This directory will be used as fallback when no other method of
 * kdbbGetFullFilename() works.
 *@see kdbbGetFullFilename
 */
#define KDB_DB_HOME              "/home"
#endif

#ifndef KDB_KEY_MOUNTPOINTS
/**Backend information.
 *
 * This key directory tells you where each backend is mounted
 * to which mountpoint. */
#define KDB_KEY_MOUNTPOINTS      "system/elektra/mountpoints"
#endif

#define KDB_KEY_MOUNTPOINTS_LEN  (sizeof (KDB_KEY_MOUNTPOINTS))

#ifndef MAX_UCHAR
#define MAX_UCHAR (UCHAR_MAX+1)
#endif

#ifndef KEYSET_SIZE
/*The minimal allocation size of a keyset inclusive
  NULL byte. ksGetAlloc() will return one less because
  it says how much can actually be stored.*/
#define KEYSET_SIZE 16
#endif

#ifndef NR_OF_PLUGINS
#define NR_OF_PLUGINS 10
#endif

#ifndef APPROXIMATE_NR_OF_BACKENDS
#define APPROXIMATE_NR_OF_BACKENDS 16
#endif


/**BUFFER_SIZE can be used as value for any I/O buffer
 * on files.
 *
 * It may be used for optimization on various
 * systems.*/
#ifndef BUFFER_SIZE
#define BUFFER_SIZE 256
#endif

#ifdef UT_NAMESIZE
#define USER_NAME_SIZE UT_NAMESIZE
#else
#define USER_NAME_SIZE 100
#endif


#ifndef DEFFILEMODE
#define DEFFILEMODE 0666
#endif

#endif
