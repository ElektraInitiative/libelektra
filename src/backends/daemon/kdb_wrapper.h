/***************************************************************************
                   kdb_wrapper.h  -  The server for the daemon backend
                             -------------------
    copyright            : (C) 2006 by Yannick Lecaillez
    email                : sizon5@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$

*/

#include "message.h"

Message *wrapper_kdbOpen(KDBHandle *handle, Message *request, uid_t remoteeuid, gid_t remoteegid);
Message *wrapper_kdbClose(KDBHandle *handle, Message *request);
Message *wrapper_kdbStatKey(KDBHandle handle, Message *request);
Message *wrapper_kdbGetKey(KDBHandle handle, Message *request);
Message *wrapper_kdbSetKey(KDBHandle handle, Message *request);
Message *wrapper_kdbSetKeys(KDBHandle handle, Message *request);
Message *wrapper_kdbRename(KDBHandle handle, Message *request);
Message *wrapper_kdbRemoveKey(KDBHandle handle, Message *request); 
Message *wrapper_kdbGetChild(KDBHandle handle, Message *request);
Message *wrapper_kdbMonitorKey(KDBHandle handle, Message *request);
Message *wrapper_kdbMonitorKeys(KDBHandle handle, Message *request); 

