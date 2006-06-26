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


#include <stdarg.h>
#include <stdlib.h> /* malloc */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "kdbbackend.h"

#include "datatype.h"
#include "argument.h"
#include "message.h"

int processRequest(Message *msg, Message *reply)
{
	int	ret;
	
	switch(msg->procId) {
		case KDB_BE_OPEN:
			ret = wrapper_kdbOpen(msg, reply);
			break:
        	case KDB_BE_CLOSE:
			ret = wrapper_kdbClose(msg, reply);
			break;
        	case KDB_BE_STATKEY:
			ret = wrapper_kdbStatKey(msg, reply);
			break;
        	case KDB_BE_GETKEY:
			ret = wrapper_kdbGetKey(msg, reply);
			break;
        	case KDB_BE_SETKEY:
			ret = wrapper_kdbSetKey(msg, reply);
			break;
        	case KDB_BE_SETKEYS:
			ret = wrapper_kdbSetKeys(msg, reply);
			break;
        	case KDB_BE_RENAME:
			ret = wrapper_kdbRename(msg, reply);
			break;
        	case KDB_BE_REMOVEKEY:
			ret = wrapper_kdbRemoveKey(msg, reply);
			break;
        	case KDB_BE_GETCHILD:
			ret = wrapper_kdbGetChild(msg, reply);
			break;
        	case KDB_BE_MONITORKEY:
			ret = wrapper_kdbMonitorKey(msg, reply);
			break;
        	case KDB_BE_MONITORKEYS:
			ret = wrapper_kdbMonitorKeys(msg, reply);
			break;
			
		default:
			ret = 1;
	}

	return ret;
}

int kdbd(int t)
{
	Message	*msg, *reply;
	uid_t	remoteeuid;
	gid_t	remoteegid;
	
	if ( ipc_eid(t, &remoteeuid, &remoteegid) == -1 ) {
		fprintf(stderr, "Can't get eUID & eGID\n");
		exit(1);
	}

	for(;;) {	
		msg = messageNew();
		reply = messageNew();
		
		protocolReadMessage(t, msg);
		processRequest(msg, reply);
		protocolSendMessage(t, msg);

		messageDel(reply);
		messageDel(msg);
	}
}

