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

$Id$

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


#define DEFAULT_BACKEND "berkeleydb"
#define BACKENDNAME "daemon"

#define REPLY_TIMEOUT 5

#ifndef SOCKET_NAME
#define SOCKET_NAME "/tmp/elektra.sock"
#endif

/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else
#define MAX_PATH_LENGTH 4096
#endif

int	sock_serv;

int checkMessage (Message *msg, ...);


typedef struct _Connection {
	int socket;
	KDBHandle handle;
} Connection;


Connection *conNew(int socket) {
	Connection *con=0;
	
	con=malloc(sizeof(Connection));
	con->socket=socket;
	
	return con;	
}


int conDel(Connection *con) {
	close(con->socket);
	free(con);
	return 0;
}


int initSocket(const char *sockPath) {
	struct sockaddr_un serv_addr;
	int sockfd;
	
	/* Create socket */
	if ( (sockfd = socket(PF_UNIX,SOCK_STREAM,0)) < 0) {
		perror("creating socket");
		return -1;
	}

	/* Bind socket */
	memset(&serv_addr, 0, sizeof(serv_addr));
	serv_addr.sun_family = AF_UNIX;
	strncpy(serv_addr.sun_path, sockPath, sizeof(serv_addr.sun_path));
	if ( bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
		perror("binding socket");
		return -1;
	}

	return sockfd;
}


int kdbOpen_server(Connection *con) {
	/* 1. really open a local backend */
	/* 2. reply success or failure to the client */
	int ret;
	Message *reply;
	Argument *returnValue;
	struct ucred credentials;
	size_t len=0;
	
	ret=kdbOpenBackend(&(con->handle),DEFAULT_BACKEND);
	
	reply=messageNew();
	reply->type=MESSAGE_REPLY;
	reply->procId=KDB_BE_OPEN;
	
	returnValue=argumentNew();
	argumentSetValue(returnValue, DATATYPE_INTEGER, &ret);
	
	messageAddArgument(reply, returnValue);
	
	protocolSendMessage(con->socket, reply);
	messageDel(reply);
	
	/* Set initialized handle credentials from the socket */
	len = sizeof(credentials);
	if ( getsockopt(con->socket, SOL_SOCKET, SO_PEERCRED, &credentials,
			&len) == -1 ) {
		perror("option");
	}

	kdbhSetPID(con->handle,credentials.pid);
	kdbhSetUID(con->handle,credentials.uid);
	kdbhSetGID(con->handle,credentials.gid);
	/*
	kdbhSetUserName(con->handle,getenv("USER"));
	kdbhSetUMask(con->handle,umask(0)); umask((*handle)->umask);
	*/

	return ret;
}


int kdbClose_server(Connection *con) {
	int ret;
	Message *reply;
	Argument *returnValue;
	
	ret=kdbClose(&(con->handle));
	
	reply=messageNew();
	reply->type=MESSAGE_REPLY;
	reply->procId=KDB_BE_CLOSE;
	
	returnValue=argumentNew();
	argumentSetValue(returnValue, DATATYPE_INTEGER, &ret);
	
	messageAddArgument(reply, returnValue);
	
	protocolSendMessage(con->socket, reply);
	messageDel(reply);
	
	return conDel(con);
}


int kdbGetKey_server(Connection *con,Argument *arg) {
	int ret;
	int error;
	Key *key;
	Message *reply;
	Argument *argument;
	
	key=(Key *)(arg->data.complexData);
	arg->data.complexData=0; /* disassociate */
	
	ret=kdbGetKey(con->handle,key);
	error=errno;
	
	reply=messageNew();
	reply->type=MESSAGE_REPLY;
	reply->procId=KDB_BE_GETKEY;
	
	/* The return value */
	argument=argumentNew();
	argumentSetValue(argument, DATATYPE_INTEGER, &ret);
	
	messageAddArgument(reply, argument);
	
	if (ret == 0) { /* success */
		argument=argumentNew();
		argumentSetValue(argument, DATATYPE_KEY, key);
	
		messageAddArgument(reply, argument);
	} else { /* failure */
		argument=argumentNew();
		argumentSetValue(argument, DATATYPE_INTEGER, &error);
	
		messageAddArgument(reply, argument);
	}
	protocolSendMessage(con->socket, reply);
	messageDel(reply);
	
	return ret;
}

int main(int argc, char **argv) {
	extern int sock_serv;
	struct sockaddr_un cli_addr;
	struct ucred credentials;
	int end=0;
	Connection *con=0;
	Message *msg, *reply;
	void *data;
	size_t dataLen;
	socklen_t len;
	int con_socket;


	/* Do some Init ... */
	sock_serv = initSocket("/tmp/elektra.sock");
	if ( sock_serv == -1 ) 
		return -1;
	if ( listen(sock_serv, 5) == -1 ) {
		perror("Listen socket");
		return -1;
	}

	/* Time to work ... */
	/* Wait for a connection */
	printf("Accepting a connection\n");
	len = sizeof(cli_addr);
	con = conNew(accept(sock_serv,(struct sockaddr *)&cli_addr, &len));
	
	/*
	 *  Read clients request
	 */
	while (! end) {
		msg = messageNew();
		
		protocolReadMessage(con->socket, msg);
		switch(msg->procId) {
			case KDB_BE_OPEN:
				printf("kdbOpen(): \n");
				kdbOpen_server(con);
				break;
			case KDB_BE_CLOSE:
				printf("kdbClose(): \n");
				kdbClose_server(con);
				end=1;
				break;
			case KDB_BE_GETKEY:
				printf("kdbGetKey(): \n");
				kdbGetKey_server(con,messageStealArgByIndex(msg,0));
				break;
			
			case KDB_BE_SETKEY:
				printf("kdbSetKey(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_LAST);
				break;
			
			case KDB_BE_STATKEY:
				printf("kdbStatKey(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_LAST);
				break;
				
			case KDB_BE_RENAME:
				printf("kdbRename(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_STRING, DATATYPE_LAST);
				break;
				
			case KDB_BE_REMOVEKEY:
				printf("kdbRemove(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_LAST);
				break;
			
			case KDB_BE_GETCHILD:
				printf("kdbGetChild(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_INTEGER, DATATYPE_LAST);
				break;
			
			case KDB_BE_MONITORKEY:
				printf("kdbMonitorKey(): \n");
				checkMessage(msg, DATATYPE_KEY, DATATYPE_INTEGER, DATATYPE_INTEGER, DATATYPE_INTEGER, DATATYPE_LAST);
				break;
			
			case KDB_BE_MONITORKEYS:
				printf("kdbMonitorKeys(): \n");
				checkMessage(msg, DATATYPE_KEYSET, DATATYPE_INTEGER, DATATYPE_INTEGER, DATATYPE_INTEGER, DATATYPE_LAST);
				break;
				
			case KDB_BE_SETKEYS:
				printf("kdbMonitorSetKeys(): \n");
				checkMessage(msg, DATATYPE_KEYSET, DATATYPE_LAST);
				break;

			default:
				printf("Unknow procedure");
				break;
		}

		messageDel(msg);
	}
}





int checkMessage(Message *msg, ...) {
	va_list		va;
	DataType	waitedType;
	int		args;

	/* Check Message */
	if ( msg == NULL ) {
		printf("<NULL>");
		return -1;
	}

        if ( msg->type != MESSAGE_REQUEST ) {
		printf("Not a request");
		return -1;
	}
	
	/* Check argument type */
	printf("check %d arguments\n", msg->nbArgs);
	args = 0;
	va_start(va, msg);
	waitedType = va_arg(va, DataType);
	while ( waitedType != DATATYPE_LAST && (args < msg->nbArgs) ) {
		printf("Checking arg %d\n", args);
		if ( msg->args[args]->type != waitedType ) {
			printf("Arg %d is type %d. Should be %d.", args, msg->args[args]->type, waitedType);
			va_end(va);
			return -1;
		}
		printf("Done\n");

		waitedType = va_arg(va, DataType);
		args++;
	}
	
	va_end(va);
	printf("Ok");

	return 0;
}

