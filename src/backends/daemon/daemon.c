/***************************************************************************
            daemon.c  -  Backend that makes RPCs to a kdb daemon
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

#include <errno.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <inttypes.h>

#include "kdbbackend.h"
#include "kdbprivate.h"


/* Backend specific includes */

#include "datatype.h"
#include "protocol.h"
#include "message.h"

#include "ipc.h"
#include "sig.h"


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

typedef struct {
	int	socketfd;
} DaemonBackendData;

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
int kdbOpen_daemon(KDBHandle *handle) {
	DaemonBackendData	*data;
	Message *request, *reply;
	int ret;

	data = (DaemonBackendData *) malloc(sizeof(DaemonBackendData));
	if ( data == NULL )
		return 1;
	memset(data, 0, sizeof(DaemonBackendData));
	
	sig_ignore(sig_pipe);
	
	data->socketfd = ipc_stream();
	if ( data->socketfd == -1 ) {
		perror("libelektra-daemon");
		free(data);
		return 1;
	}
	if ( ipc_connect(data->socketfd, SOCKET_NAME) == -1 ) {
		perror("libelektra-daemon");
		close(data->socketfd);
		free(data);
		return 1;
	}
	ndelay_off(data->socketfd);
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_OPEN, DATATYPE_LAST);
	if ( request == NULL ) {
		fprintf(stderr, "Error building request\n");
		close(data->socketfd);
		free(data);
		messageDel(request);
		return 1;
	}
		
	/* Send request */
	if ( protocolSendMessage(data->socketfd, request) == -1 ) {
		fprintf(stderr, "Error sending message\n");
		close(data->socketfd);
		free(data);
		messageDel(request);
		return 1;
	}
	messageDel(request);

	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		close(data->socketfd);
		messageDel(reply);
		free(data);
		return 1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		close(data->socketfd);
		messageDel(reply);
		free(data);
		return 1;
	}
	
	/* Get reply value */
	if ( messageExtractArgs(reply, DATATYPE_INTEGER, &ret, DATATYPE_INTEGER, &errno, DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting args\n");
		close(data->socketfd);
		messageDel(reply);
		free(data);
		return 1;
	}
	
	/* Instrument the handle to modify the backend name */
	/*
	name=kdbhGetBackendName(handle);
	// name will point to "daemon" here, but we don't need it anymore.
	free(name);
	// get the backend name being used by the daemon from the reply...
	// ...
	name=malloc(SIZE);
	sprintf(name,"daemon+%s",daemon_real_backend);
	// set the new name:
	kdbhSetBackendName(handle,name);
	// at this point, the new backend name will be something like "daemon+berkeleydb"
	*/
	
	messageDel(reply);
	
	kdbhSetBackendData(*handle, data);

	return ret;
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
int kdbClose_daemon(KDBHandle *handle)
{
	DaemonBackendData *data;
	Message	*request, *reply;
	int	ret = 0;

	data = (DaemonBackendData *) kdbhGetBackendData(*handle);
	if ( data == NULL )
		return 0;
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_CLOSE,
				DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbClose_daemon");
		return 1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret ) {
		kdbhSetBackendData(*handle, NULL);
		close(data->socketfd); 
		free(data);
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		kdbhSetBackendData(*handle, NULL);
		close(data->socketfd);
		free(data);
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		close(data->socketfd);
		messageDel(reply);
		free(data);
		return 1;
	}
	
	/* Get reply value */
	if ( messageExtractArgs(reply, DATATYPE_INTEGER, &ret, DATATYPE_INTEGER, &errno, DATATYPE_LAST) == -1 ) {
		kdbhSetBackendData(*handle, NULL);
		close(data->socketfd);
		free(data);
		messageDel(reply);
		return -1;
	} 
	messageDel(reply);

	kdbhSetBackendData(*handle, NULL);
	close(data->socketfd);
	free(data);
	
	return ret;
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
int kdbStatKey_daemon(KDBHandle handle, Key *key) 
{
	Key	*workKey, copy;
	DaemonBackendData *data;
	Message *request, *reply;
	int     ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL ) 
		return -1;
	
	request = messageNew(MESSAGE_REQUEST, KDB_BE_STATKEY,
			DATATYPE_KEY, key,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbStatKey_daemon");
		return -1;
	}
		

	/* Send request */
        ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
        if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return -1;
	}

        /* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return -1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEY, key,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return 1;
	}
	messageDel(reply);

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
int kdbGetKey_daemon(KDBHandle handle, Key *key)
{
	DaemonBackendData	*data;
	Message         *request, *reply;
	int             ret;

	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL ) 
		return 1;
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_GETKEY,
			DATATYPE_KEY, key,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbGetKey_daemon");
		return -1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return -1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEY, key,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

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
int kdbSetKey_daemon(KDBHandle handle, Key *key)
{
	DaemonBackendData *data;
	Message *request, *reply;
	int ret;
       
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
 	if ( data == NULL )
 		return 1;
      
	fprintf(stderr, "kdbSetKey(%s:%s)\n", keyStealOwner(key), keyStealName(key));
       
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_SETKEY,
			DATATYPE_KEY, key,
 			DATATYPE_LAST);
	if ( request == NULL ) {
 		perror("kdbSetKey_daemon");
 		return -1;
 	}
 	
 	/* Send request */
 	ret = protocolSendMessage(data->socketfd, request);
 	messageDel(request);
 	if ( ret == -1 ) {
 		fprintf(stderr, "Error writing message\n");
 		return 1;
 	}
 	
 	/* Wait for a reply for 5 secondes */
 	reply = protocolReadMessage(data->socketfd);
 	if ( reply == NULL ) {
 		fprintf(stderr, "Error reading message\n");
 		messageDel(reply);
 		return -1;
 	}
	
 	/* Check for Internal error */
 	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
 		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
 		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
 		messageDel(reply);
 		return 1;
 	}
  	
 	/* Get result */
 	if ( messageExtractArgs(reply,
 				DATATYPE_INTEGER, &ret,
 				DATATYPE_INTEGER, &errno,
 				DATATYPE_KEY, key,
 				DATATYPE_LAST) ) {
 		fprintf(stderr, "Error extracting ARGS\n");
 		messageDel(reply);
 		return -1;
 	}
	messageDel(reply);
 	
	return ret;	
}



/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_daemon(KDBHandle handle, Key *key, const char *newName)
{
	DaemonBackendData *data;
	Message *request, *reply;
	int ret;
	
 	data = (DaemonBackendData *) kdbhGetBackendData(handle);
 	if ( data == NULL )
		return 1;
	
 	/* Prepare request */
 	request = messageNew(MESSAGE_REQUEST, KDB_BE_RENAME,
			DATATYPE_KEY, key,
			DATATYPE_STRING, newName,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbRename_daemon");
		return -1;
	}
	
 	/* Send request */
 	ret = protocolSendMessage(data->socketfd, request);
 	messageDel(request);
 	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
 	/* Wait for a reply for 5 secondes */
 	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
 	/* Get result */
 	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEY, key,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

        return ret;	
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_daemon(KDBHandle handle, const Key *key)
{
	Key	copy;
	DaemonBackendData *data;
	Message *request, *reply;
	int ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL )
		return 1;
	
	/* Prepare request */
	keyInit(&copy);
	if ( keyDup(key, &copy) ) {
		keyClose(&copy);
		return -1;
	}
	request = messageNew(MESSAGE_REQUEST, KDB_BE_REMOVEKEY,
			DATATYPE_KEY, &copy,
			DATATYPE_LAST);
	keyClose(&copy);
	if ( request == NULL ) {
		perror("kdbRemoveKey_daemon");
		return 1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);
	
	return ret;
}

/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_daemon(KDBHandle handle, const Key *parentKey, KeySet *returned, unsigned long options) 
{
	Key	copy;
	DaemonBackendData       *data;
	Message         *request, *reply;
	int             ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL )
		return 1;
	
	/* Prepare request */
	keyInit(&copy);
	if ( keyDup(parentKey, &copy) ) {
		keyClose(&copy);
		return -1;
	}
	request = messageNew(MESSAGE_REQUEST, KDB_BE_GETCHILD,
			DATATYPE_KEY, &copy,
			DATATYPE_ULONG, &options,
			DATATYPE_LAST);
	keyClose(&copy);
	if ( request == NULL ) {
		perror("kdbGetKeyChildKeys_daemon");
		return -1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEYSET, returned,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

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
int kdbSetKeys_daemon(KDBHandle handle, KeySet *ks) 
{
	DaemonBackendData       *data;
	Message         *request, *reply;
	int             ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL )
		return 1;
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_SETKEYS,
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbSetKeys_daemon");
		return -1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEYSET, ks,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

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
u_int32_t kdbMonitorKeys_daemon(KDBHandle handle, KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep)
{
	DaemonBackendData       *data;
	Message         *request, *reply;
	unsigned long	monitorRet;
	int             ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL )
		return 1;
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_MONITORKEYS,
			DATATYPE_KEYSET, interests,
			DATATYPE_ULONG, &diffMask,
			DATATYPE_ULONG, &iterations,
			DATATYPE_ULONG, &sleep,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbMonitorKeys_daemon");
		return 1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_ULONG, &monitorRet,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEYSET, interests,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

	return monitorRet;
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
u_int32_t kdbMonitorKey_daemon(KDBHandle handle, Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) 
{
        DaemonBackendData       *data;
	Message         *request, *reply;
	unsigned long   monitorRet;
	int             ret;
	
	data = (DaemonBackendData *) kdbhGetBackendData(handle);
	if ( data == NULL )
		return 1;
	
	/* Prepare request */
	request = messageNew(MESSAGE_REQUEST, KDB_BE_MONITORKEY,
			DATATYPE_KEY, interest,
			DATATYPE_ULONG, &diffMask,
			DATATYPE_ULONG, &iterations,
			DATATYPE_ULONG, &sleep,
			DATATYPE_LAST);
	if ( request == NULL ) {
		perror("kdbMonitorKey_daemon");
		return 1;
	}
	
	/* Send request */
	ret = protocolSendMessage(data->socketfd, request);
	messageDel(request);
	if ( ret == -1 ) {
		fprintf(stderr, "Error writing message\n");
		return 1;
	}
	
	/* Wait for a reply for 5 secondes */
	reply = protocolReadMessage(data->socketfd);
	if ( reply == NULL ) {
		fprintf(stderr, "Error reading message\n");
		messageDel(reply);
		return -1;
	}

	/* Check for Internal error */
	if ( messageGetProcedure(reply) == INTERNAL_ERROR ) {
		messageExtractArgs(reply, DATATYPE_INTEGER, &ret);
		fprintf(stderr, "An error occured in kdbd: %d.\n", ret);
		messageDel(reply);
		return 1;
	}
	
	/* Get result */
	if ( messageExtractArgs(reply,
				DATATYPE_ULONG, &monitorRet,
				DATATYPE_INTEGER, &errno,
				DATATYPE_KEY, interest,
				DATATYPE_LAST) ) {
		fprintf(stderr, "Error extracting ARGS\n");
		messageDel(reply);
		return -1;
	}
	messageDel(reply);

	return monitorRet;
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
KDBEXPORT(daemon)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_daemon,
		KDB_BE_CLOSE,          &kdbClose_daemon,
		KDB_BE_GETKEY,         &kdbGetKey_daemon,
		KDB_BE_SETKEY,         &kdbSetKey_daemon,
		KDB_BE_STATKEY,        &kdbStatKey_daemon,
		KDB_BE_RENAME,         &kdbRename_daemon,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_daemon,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_daemon,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_daemon,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_daemon,
		KDB_BE_SETKEYS,        &kdbSetKeys_daemon,
		KDB_BE_END);
}
