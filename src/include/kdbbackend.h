/***************************************************************************
                kdbbackend.h  -  Methods for backend programing
                             -------------------
    begin                : Mon Dec 25 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: kdbbackend.h 200 2005-03-21 21:54:41Z aviram $
$LastChangedBy: aviram $

*/

#ifndef KDBBACKEND_H
#define KDBBACKEND_H

#include <kdb.h>
#include <kdbprivate.h>



typedef struct _KDBBackend KDBBackend;


/**
 * Switches to denote the backend methods. Used in calls to kdbBackendExport().
 * 
 * @ingroup backend
 */
enum KDBBackendMethod {
	KDB_BE_OPEN=1,               /*!< Next arg is backend for kdbOpen() */
	KDB_BE_CLOSE=1<<1,           /*!< Next arg is backend for kdbClose() */
	KDB_BE_STATKEY=1<<2,         /*!< Next arg is backend for kdbStatKey() */
	KDB_BE_GETKEY=1<<3,          /*!< Next arg is backend for kdbGetKey() */
	KDB_BE_SETKEY=1<<4,          /*!< Next arg is backend for kdbSetKey() */
	KDB_BE_SETKEYS=1<<5,         /*!< Next arg is backend for kdbSetKeys() */
	KDB_BE_RENAME=1<<6,          /*!< Next arg is backend for kdbRename() */
	KDB_BE_REMOVEKEY=1<<7,       /*!< Next arg is backend for kdbRemoveKey() */
	KDB_BE_GETCHILD=1<<8,        /*!< Next arg is backend for kdbGetKeyChildKeys() */
	KDB_BE_MONITORKEY=1<<9,      /*!< Next arg is backend for kdbMonitorKey() */
	KDB_BE_MONITORKEYS=1<<10,    /*!< Next arg is backend for kdbMonitorKeys() */
	KDB_BE_END=0                 /*!< End of arguments */
};


#ifdef __cplusplus
extern "C" {
#endif

KDBBackend *kdbBackendExport(const char *backendName, ...);

typedef KDBBackend *(*KDBBackendFactory)(void);

/* Let the backend be aware of default implementations we provide */
int kdbSetKeys_default(KeySet *ks);
u_int32_t kdbMonitorKeys_default(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep);
u_int32_t kdbMonitorKey_default(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep);


#ifdef __cplusplus
}
#endif





#endif /* KDBBACKEND_H */
