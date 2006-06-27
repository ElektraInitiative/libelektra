/***************************************************************************
                   kdbd.c  -  The server for the daemon backend
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2005 by Yannick Lecaillez
    email                : yl@itioweb.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: kdbd.c 788 2006-05-29 16:30:00Z aviram $

*/

#include "kdbbackend.h"

#include "protocol.h"
#include "message.h"
#include "kdb_wrapper.h"

#define	HANDLE_PREALLOC	5

static Message *processRequest(Message *request, uid_t remoteeuid, gid_t remoteegid);

static Message *processRequest(Message *request, uid_t remoteeuid, gid_t remoteegid)
{
	int	msgType, procedure;
	int	ret;

	msgType = messageGetType(request);
	if ( msgType != MESSAGE_REQUEST ) {
		fprintf(stderr, "processRequest(): Received a non-request message %d.\n", procedure);
		return NULL;
	}
	
	procedure = messageGetProcedure(request);
	switch(procedure) {
		case KDB_BE_OPEN:	return wrapper_kdbOpen(request, remoteeuid, remoteegid);
        	case KDB_BE_CLOSE:	return wrapper_kdbClose(request);
        	case KDB_BE_STATKEY:	return wrapper_kdbStatKey(request);
        	case KDB_BE_GETKEY:	return wrapper_kdbGetKey(request);
        	case KDB_BE_SETKEY:	return wrapper_kdbSetKey(request);
        	case KDB_BE_SETKEYS:	return wrapper_kdbSetKeys(request);
        	case KDB_BE_RENAME:	return wrapper_kdbRename(request);
        	case KDB_BE_REMOVEKEY:	return wrapper_kdbRemoveKey(request);
        	case KDB_BE_GETCHILD:	return wrapper_kdbGetChild(request);
        	case KDB_BE_MONITORKEY:	return wrapper_kdbMonitorKey(request);
        	case KDB_BE_MONITORKEYS:	return wrapper_kdbMonitorKeys(request);
		default:
			return NULL;
	}

	return NULL;
}

int kdbd(int t)
{
	Message	*request, *reply;
	uid_t   remoteeuid;
	gid_t   remoteegid;
	int	closed;
	
	if ( ipc_eid(t, &remoteeuid, &remoteegid) == -1 ) {
		fprintf(stderr, "Can't get eUID & eGID\n");
		return 1;
	}
	
	closed = 0;
	while ( !closed ) {
		request = protocolReadMessage(t);
		closed = (messageGetProcedure(request) == KDB_BE_CLOSE);
		
		reply = processRequest(request, remoteeuid, remoteegid);
		messageDel(request);
		
		protocolSendMessage(t, reply);
		messageDel(reply);
	}

	return 0;
}

