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




#ifdef __cplusplus
extern "C" {
#endif

KDBBackend *kdbBackendExport(
	int (*kdbOpen)(),
	int (*kdbClose)(),
	
	int (*kdbGetKey)(Key *),
	int (*kdbSetKey)(Key *),
	int (*kdbStatKey)(Key *),
	int (*kdbRename)(Key *, const char *),
	int (*kdbRemove)(const char *),
	int (*kdbGetKeyChildKeys)(const Key *, KeySet *, unsigned long),

	
	/* These are the optional methods */
	
	int (*kdbSetKeys)(KeySet *),
	u_int32_t (*kdbMonitorKey)(Key *, u_int32_t,unsigned long, unsigned),
	u_int32_t (*kdbMonitorKeys)(KeySet *, u_int32_t,unsigned long, unsigned)
);



typedef KDBBackend *(*KDBBackendFactory)(void);


#ifdef __cplusplus
}
#endif





#endif /* KDBBACKEND_H */
