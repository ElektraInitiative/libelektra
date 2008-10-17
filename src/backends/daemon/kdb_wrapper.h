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

Message *wrapper_kdbOpen(KDB *handle, Message *request, uid_t remoteeuid, gid_t remoteegid);
Message *wrapper_kdbClose(KDB *handle, Message *request);
Message *wrapper_kdbStatKey(KDB *handle, Message *request);
Message *wrapper_kdbGetKey(KDB *handle, Message *request);
Message *wrapper_kdbSetKey(KDB *handle, Message *request);
Message *wrapper_kdbSetKeys(KDB *handle, Message *request);
Message *wrapper_kdbRename(KDB *handle, Message *request);
Message *wrapper_kdbRemoveKey(KDB *handle, Message *request); 
Message *wrapper_kdbGetChild(KDB *handle, Message *request);
Message *wrapper_kdbMonitorKey(KDB *handle, Message *request);
Message *wrapper_kdbMonitorKeys(KDB *handle, Message *request); 

