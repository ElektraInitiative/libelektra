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
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: $
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
	KDB_BE_REMOVE=1<<7,          /*!< Next arg is backend for kdbRemove() */
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


#ifdef __cplusplus
}
#endif





#endif /* KDBBACKEND_H */
