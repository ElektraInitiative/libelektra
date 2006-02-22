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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <inttypes.h>

#include "datatype.h"
#include "argument.h"
#include "message.h"

#include "protocol.h"

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

int socketfd = -1;
struct sockaddr_un sockserver;

/**
 * @defgroup backend Elektra framework for pluggable backends
 * @brief The tactics to create pluggable backends to libelektra.so
 *
 * Since version 0.4.9, Elektra can dynamically load different key storage
 * backends. Fast jump to kdbBackendExport() to see an example of a backend
 * implementation.
 * 
 * The methods of class KeyDB that are backend dependent are kdbOpen(),
 * kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename(). So a backend must
 * reimplement these methods.
 * 
 * And methods that have a builtin default high-level inefficient
 * implementation are kdbSetKeys(), kdbMonitorKey(), kdbMonitorKeys(). So
 * it is suggested to reimplement them too, to make them more efficient.
 *
 * The other KeyDB methods are higher level. They use the above methods to
 * do their job, and generally don't have to be reimplemented for a
 * different backend.
 * 
 * The backend must implement a method with name kdbBackendFactory() and no
 * parameters, that is responsible of exporting the implementations of 
 * libelektra.so backend dependent methods.
 * 
 * The backend implementation must:
 * @code
#include <kdb.h>
#include <kdbbackend.h>
 * @endcode
 * 
 * <b>Better than that, a skeleton of a backend implementation is provided inside
 * Elektra development package or source code tree, and should be used as a
 * base for the implementation.</b>
 * 
 * An elektrified program will use the backend defined by environment variable
 * @e $KDB_BACKEND, The backend library is dynamically loaded when the program
 * calls kdbOpen(), unless if the program is security/authentication/setuid
 * related, in which it probably uses the more secure kdbOpenDefault() which
 * completely ignores the @e $KDB_BACKEND environment and will use the
 * @c "default" named backend defined by the sysadmin. Look at
 * @c /lib/libelektra-default.so link to see the default backend for your
 * system.
 * 
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend, and we'll document this skeleton here.
 * 
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libelektra.so look for its library as @c libelektra-BACKENDNAME.so.
 * 
 * Elektra source code tree includes several backend implementations
 * (http://germane-software.com/repositories/elektra/trunk/src/backends)
 * that can also be used as a reference.
 */



int connectToDaemon(const char *sockFname)
{
	extern int socketfd;
		int	ret;

	/* Connected yet */
	if ( socketfd != -1 )
		return 0;
	
	//
        // Open socket
	// 
	socketfd = socket(AF_UNIX, SOCK_STREAM, 0);
	if ( socketfd == -1 ) {
		perror("connectToDaemon");
		return 1;
	}

	//
	// Connect
	//
	sockserver.sun_family = AF_UNIX;
	strcpy(sockserver.sun_path, SOCKET_NAME);
	ret = connect(socketfd, (struct sockaddr *) &sockserver,
		       	sizeof(sockserver.sun_family) + strlen(sockserver.sun_path));
	if ( ret ) {
		perror("connectToDaemon");
		return 1;
	}

	return 0;
}

/**
 * Initialize the backend.
 * This is the first method kdbOpenBackend() calls after dynamically loading
 * the backend library.
 *
 * This method is responsible of:
 * - backend's specific configuration gathering
 * - all backend's internal structs initialization
 * - initial setup of all I/O details such as opening a file, connecting to a
 *   database, etc
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbOpenBackend()
 * @see kdbOpen()
 * @ingroup backend
 */
int kdbOpen_daemon()
{
	extern	int	socketfd;
		Message	*request;
		Message	reply;
		ssize_t	ret;

	printf("kdbOpen()\n");
		
	//
	// Establish connection
	//
	if ( connectToDaemon(SOCKET_NAME) )
		return 1;
	
	//
	// TODO: Handshake
	//
	// if ( connectionHandshake() )
	//	return 1;
	
	//
	// kdbOpen()
	//
		        
	/* Prepare request */
	request = messageNewRequest(KDB_BE_OPEN, DATATYPE_LAST);
        if ( request == NULL ) {
		fprintf(stderr, "error new request\n");
		return 1;
	}
		
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
	//	printf(stderr, "error send message\n");
		return 1;
	}

        /* Wait for a reply for 5 secondes */
#ifdef TOTO
	messageInit(&reply);
	if ( protocolReadMessage(&reply, REPLY_TIMEOUT) == -1 )
		return 1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return 1;
#endif
	
	return 0;
}




/**
 * All finalization logic of the backend should go here.
 * 
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 * Should free any memory that the backend no longer needs.
 * After this call, libelektra.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_daemon()
{
	extern	int	socketfd;
		Message	*request;
       		Message	reply;
		int	ret;

	printf("kdbClose() %d\n", socketfd);
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_CLOSE, DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	close(socketfd);
	return 0; /* success */
}

/**
 * Implementation for kdbStatKey() method.
 *
 * This method is responsible of:
 * - make necessary I/O to retrieve @p key->name's metadata
 * - fill the @p key struct with its metadata
 *
 * @see kdbStatKey() for expected behavior.
 * @ingroup backend
 */
int kdbStatKey_daemon(Key *key) {
	extern	int	socketfd;
		Message		*request;
		Message		reply;
		int		ret;

	printf("kdbStatKey()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_STATKEY,
				DATATYPE_KEY, key,
				DATATYPE_LAST); 
	if ( request == NULL )
		return 1;

	/* Send request */	
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret ) 
		return 1;

	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 ) 
		return -1;

	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 ) 
		return -1;
		
	return ret;
}


/**
 * Implementation for kdbGetKey() method.
 *
 * This method is responsible of:
 * - make necessary I/O to retrieve all @p key->name's value and metadata
 * - fill the @p key struct with its value and metadata
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
int kdbGetKey_daemon(Key *key)
{
	extern	int	socketfd;
		Message         *request;
        	Message         reply;
		int             ret;

	printf("kdbGetKey()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_GETKEY,
			DATATYPE_KEY, key,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get result */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;

	if ( messageGetArgumentValue(reply, 1, DATATYPE_KEY, key) == -1 )
                return -1;
	
	return ret;
}

/**
 * Implementation for kdbSetKey() method.
 *
 * This method is responsible of:
 * - check the existence of @p key->name on persistent storage
 * - prepare the backend to receive a new or updated key
 * - use value and metadata from @p key to store them in the backend storage
 * - fill the @p key struct with its value and metadata
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_daemon(Key *key)
{
	extern	int	socketfd;
		Message         *request;
        	Message         reply;
		int             ret;

	printf("kdbSetKey()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_SETKEY,
			DATATYPE_KEY, key,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;	
}



/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_daemon(Key *key, const char *newName)
{
	extern	int	socketfd;
		Message	*request;
		Message	reply;
		int	ret;

	printf("kdbRename()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_RENAME,
			DATATYPE_KEY, key,
			DATATYPE_STRING, newName,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_daemon(const Key *key)
{
	extern	int	socketfd;
		Message         *request;
		Message         reply;
		int             ret;
	
	printf("kdbRemoveKey()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_REMOVEKEY,
			DATATYPE_KEY, key,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;
}




/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_daemon(const Key *parentKey, KeySet *returned, unsigned long options) 
{
	extern	int	socketfd;
		Message         *request;
		Message         reply;
		int             ret;
	
	printf("kdbGetKeyChildKeys()\n");
		
	/* Prepare request */
	printf("\t ->");
	request = messageNewRequest(KDB_BE_GETCHILD,
			DATATYPE_KEY, parentKey,
			DATATYPE_INTEGER, options,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;

	/* Send request */
	printf("\t ->");
	ret = protocolSendMessage(socketfd, request);
	printf("\t ->");
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
		
	/* messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1; */
	
	/* Get reply value 
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;

	if ( messageGetArgumentValue(reply, 1, DATATYPE_KEYSET, returned) == -1 )
		return -1;
	*/
	return ret;
}


/**
 * Implementation for kdbSetKeys() method.
 * 
 * The implementation of this method is optional, and a builtin, probablly 
 * inefficient implementation can be explicitly used when exporting the
 * backend with kdbBackendExport(), using kdbSetKeys_default().
 * 
 * @see kdbSetKeys() for expected behavior.
 * @ingroup backend
 */
int kdbSetKeys_daemon(KeySet *ks) 
{
	extern	int	socketfd;
		Message         *request;
		Message         reply;
		Argument        *arg;
		int             ret;

	printf("kdbSetKeys()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_SETKEYS,
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST);

	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKeys_daemon(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep)
{
	extern	int	socketfd;
		Message         *request;
		Message         reply;
		Argument        *arg;
		int             ret;

	printf("kdbMonitorKeys()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_MONITORKEYS,
			DATATYPE_KEYSET, interests,
			DATATYPE_INTEGER, diffMask,
			DATATYPE_INTEGER, iterations,
			DATATYPE_INTEGER, sleep,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;
}



/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKey_daemon(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) 
{
	extern	int	socketfd;
		Message         *request;
       	 	Message         reply;
		Argument        *arg;
		int             ret;
	
	printf("kdbMonitorKey()\n");
		
	/* Prepare request */
	request = messageNewRequest(KDB_BE_MONITORKEY,
			DATATYPE_KEY, 		interest,
			DATATYPE_INTEGER,	diffMask,
			DATATYPE_INTEGER,	iterations,
			DATATYPE_INTEGER,	sleep,
			DATATYPE_LAST);
	if ( request == NULL )
		return 1;
	
	/* Send request */
	ret = protocolSendMessage(socketfd, request);
	messageDel(request);
	if ( ret )
		return 1;
	
	/* Wait for a reply for 5 secondes */
	messageInit(&reply);
	if ( readMessage(&reply, REPLY_TIMEOUT) == -1 )
		return -1;
	
	/* Get reply value */
	if ( messageGetArgumentValue(reply, 0, DATATYPE_INTEGER, &ret) == -1 )
		return -1;
	
	return ret;
}


/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup backend
 */
KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_daemon,
		KDB_BE_CLOSE,          &kdbClose_daemon,
		KDB_BE_GETKEY,         &kdbGetKey_daemon,
		KDB_BE_SETKEY,         &kdbSetKey_daemon,
		KDB_BE_STATKEY,        &kdbStatKey_daemon,
		KDB_BE_RENAME,         &kdbRename_daemon,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_daemon,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_daemon,
		/* set to default implementation: */
		KDB_BE_MONITORKEY,     &kdbMonitorKey_daemon,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_daemon,
		KDB_BE_SETKEYS,        &kdbSetKeys_daemon,
		KDB_BE_END);
}
