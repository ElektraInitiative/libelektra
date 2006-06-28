/***************************************************************************
                   kdbd.c  -  The server for the daemon backend
                             -------------------
    copyright            : (C) 2006 by Yannick Lecaille
    email                : sizon5@gmail.com
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

static Message *processRequest(Message *request, KDBHandle *handle, uid_t remoteeuid, gid_t remoteegid);

static Message *processRequest(Message *request, KDBHandle *handle, uid_t remoteeuid, gid_t remoteegid)
{
	Message	*reply;
	int	msgType, procedure;
	int	ret;

	msgType = messageGetType(request);
	if ( msgType != MESSAGE_REQUEST ) {
		fprintf(stderr, "processRequest(): Received a non-request message %d.\n", procedure);
		return NULL;
	}
	
	procedure = messageGetProcedure(request);
	switch(procedure) {
		case KDB_BE_OPEN:
			reply = wrapper_kdbOpen(handle, request, remoteeuid, remoteegid);
			break;
			
        	case KDB_BE_CLOSE:
			reply = wrapper_kdbClose(handle, request);
			break;
			
        	case KDB_BE_STATKEY:
			reply = wrapper_kdbStatKey(*handle, request);
			break;
			
        	case KDB_BE_GETKEY:
			reply = wrapper_kdbGetKey(*handle, request);
			break;
			
        	case KDB_BE_SETKEY:
			reply = wrapper_kdbSetKey(*handle, request);
			break;
			
        	case KDB_BE_SETKEYS:
			reply = wrapper_kdbSetKeys(*handle, request);
			break;
			
        	case KDB_BE_RENAME:
			reply = wrapper_kdbRename(*handle, request);
			break;
			
        	case KDB_BE_REMOVEKEY:
			reply = wrapper_kdbRemoveKey(*handle, request);
			break;
			
        	case KDB_BE_GETCHILD:
			reply = wrapper_kdbGetChild(*handle, request);
			break;
			
        	case KDB_BE_MONITORKEY:
			reply = wrapper_kdbMonitorKey(*handle, request);
			break;
			
        	case KDB_BE_MONITORKEYS:
			reply = wrapper_kdbMonitorKeys(*handle, request);
			break;
			
		default:
			reply = NULL;
	}

	if ( reply == NULL ) {
		/* Internat error from wrapper */
		fprintf(stderr, "Internal error\n");
		reply = messageNew(MESSAGE_REPLY, INTERNAL_ERROR,
					DATATYPE_INTEGER, &errno,
					DATATYPE_LAST);
		if ( reply == NULL )
			fprintf(stderr, "An internal error caused a fatal error. Client will be confused :-(.\n"); 
	}
	
	return reply;
}

int kdbd(void *pIntThreadHandle)
{
	KDBHandle	handle;
	int		threadHandle, socketFd;
	Message		*request, *reply;
	uid_t   	remoteeuid;
	gid_t   	remoteegid;
	int		closed;
	
	threadHandle = *((int *) pIntThreadHandle);
	if ( (socketFd = threadGetSocket(threadHandle)) == -1 ) {
		fprintf(stderr, "Can't get socket :-(\n");
		return 1;
	}

	if ( ipc_eid(socketFd, &remoteeuid, &remoteegid) == -1 ) {
		fprintf(stderr, "Can't get eUID & eGID\n");
		return 1;
	}
	
	closed = 0;
	while ( !closed ) {
		request = protocolReadMessage(socketFd);
		if ( request == NULL ) {
			if ( errno == EPIPE ) {
				/* Client closed the connection */
				threadExit(threadHandle, NULL);
			} else {	
				/* They are probably some usefull errno
				 * to check here ...
				 */
			
				perror("kdbd");
				continue;
			}
		}
		closed = (messageGetProcedure(request) == KDB_BE_CLOSE);
		
		reply = processRequest(request, &handle, remoteeuid, remoteegid);
		messageDel(request);

		protocolSendMessage(socketFd, reply);
		messageDel(reply);
	}

	threadExit(threadHandle, NULL);
	
	return 0;
}

