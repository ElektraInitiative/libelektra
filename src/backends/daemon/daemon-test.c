/***************************************************************************
            daemon.c  -  Backends which communication with Elektra Daemon
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

#include <stdarg.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "datatype.h"
#include "argument.h"
#include "message.h"

#include "kdb.h"
#include "kdbbackend.h"

#define BACKENDNAME "daemon"

#define REPLY_TIMEOUT	5

#ifndef SOCKET_NAME
#define SOCKET_NAME	"/tmp/elektra.sock"
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

int checkMessage	(Message *msg, ...);
	

int initSocket(const char *sockPath)
{
	struct	sockaddr_un	serv_addr;
		int		sockfd;
	
	/* Create socket */
	if ( (sockfd = socket(AF_UNIX,SOCK_STREAM,0)) < 0) {
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


int main()
{
	extern	int		sock_serv;
	struct	sockaddr_un	cli_addr;
	struct	ucred		credentials;
		Message		*msg, *reply;
		void		*data;
		size_t		dataLen;
		socklen_t	len;
		int		con_socket;


	/* Do some Init ... */
	sock_serv = initSocket("/tmp/elektra.sock");
	if ( sock_serv == -1 ) 
		return -1;
	if ( listen(sock_serv, 5) == -1 ) {
		perror("Listen socket");
		return -1;
	}

	/* Time to work ... */	
	for(;;) {
		/* Wait for a connection */
		printf("Accepting a connection\n");
		len = sizeof(cli_addr);
		con_socket = accept(sock_serv, (struct sockaddr *) &cli_addr, &len);
		if ( con_socket == -1 ) {
			perror("accepting");
			continue;
		}	

		/* Get credentials of client */
		len = sizeof(credentials);
		if ( getsockopt(con_socket, SOL_SOCKET, SO_PEERCRED, &credentials, &len) == -1 ) {
			perror("option");
			close(con_socket);
			continue;
		}
		printf("Accepted connection from PID=%d UID=%d GID=%d\n", credentials.pid, credentials.uid, credentials.gid);

		/*
		 *  Read clients request 
		 */
		for(;;) {
			msg = messageNew();
			
			protocolReadMessage(con_socket, msg);	

			switch(msg->procId) {
				case KDB_BE_OPEN:
					printf("kdbOpen(): \n");
					checkMessage(msg, DATATYPE_LAST);
					break;
				case KDB_BE_CLOSE:
					printf("kdbClose(): \n");
					checkMessage(msg, DATATYPE_LAST);
					break;	
				
				case KDB_BE_GETKEY:
					printf("kdbGetKey(): \n");
					checkMessage(msg, DATATYPE_KEY, DATATYPE_LAST);
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
		end:
		printf("disconnected\n");
	}

	close(con_socket);
}

int checkMessage(Message *msg, ...)
{
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

