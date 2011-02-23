/***************************************************************************
                   kdbd.c  -  The server for the daemon backend
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

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include "kdbbackend.h"

#include "protocol.h"
#include "message.h"
#include "kdb_wrapper.h"
#include "ipc.h"
#include "thread.h"

static Message *processRequest(Message *request, KDB *handle, uid_t remoteeuid, gid_t remoteegid);

static Message *processRequest(Message *request, KDB *handle, uid_t remoteeuid, gid_t remoteegid)
{
	Message	*reply;
	int	msgType, procedure;

	msgType = messageGetType(request);
	if ( msgType != MESSAGE_REQUEST ) {
		fprintf(stderr, "processRequest(): Received a non-request message.\n");
		return NULL;
	}
	
	procedure = messageGetProcedure(request);
	switch(procedure) {
/*		case KDB_BE_OPEN:
			reply = wrapper_kdbOpen(handle, request, remoteeuid, remoteegid);
			break;
			
        	case KDB_BE_CLOSE:
			reply = wrapper_kdbClose(handle, request);
			break;*/
			
        	case KDB_BE_STATKEY:
			reply = wrapper_kdbStatKey(handle, request);
			break;
			
        	case KDB_BE_GETKEY:
			reply = wrapper_kdbGetKey(handle, request);
			break;
			
        	case KDB_BE_SETKEY:
			reply = wrapper_kdbSetKey(handle, request);
			break;
			
        	case KDB_BE_SETKEYS:
			reply = wrapper_kdbSetKeys(handle, request);
			break;
			
        	case KDB_BE_GETCHILD:
			reply = wrapper_kdbGetChild(handle, request);
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
	KDB	*handle;
	int		threadHandle, socketFd;
	Message		*request, *reply;
	uid_t   	remoteeuid;
	gid_t   	remoteegid;
	pid_t		remotepid;
	int		closed;

	handle = kdbOpen();

	pthread_cleanup_push(threadExit, pIntThreadHandle);
	
	threadHandle = *((int *) pIntThreadHandle);
	
	if ( (socketFd = threadGetSocket(threadHandle)) == -1 ) {
		fprintf(stderr, "Can't get socket :-(\n");
		return 1;
	} 

	if ( ipc_eid(socketFd, &remoteeuid, &remoteegid, &remotepid) == -1 ) {
		perror("kdbd");
		return 1;
	}
	fprintf(stderr, "Thread %ld launched to serve PID %d (euid=%d/egid=%d)\n", pthread_self(), remotepid, remoteeuid, remoteegid);

	closed = 0;
	while ( !closed ) {
		request = protocolReadMessage(socketFd);
		if ( request == NULL ) {
			if ( (errno == EPIPE) || (errno == EINVAL) ) {
				/* Client closed the connection or
				 * malformed request */
				messageDel(request);
				return 1;
			} else {
				/* They are probably some usefull errno
				 * to check here ...
				 */
				continue;
			}
		}
		closed = (messageGetProcedure(request) == KDB_BE_CLOSE);
		
		reply = processRequest(request, &handle, remoteeuid, remoteegid);
		messageDel(request);

		protocolSendMessage(socketFd, reply);
		messageDel(reply);
	}

	pthread_cleanup_pop(1);

	kdbClose (handle);
	
	return 0;
}

