/***************************************************************************
                   kdb_wrapper.c  -  The server for the daemon backend
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

$Id: kdbd.c 788 2006-05-29 16:30:00Z aviram $

*/


#include <stdlib.h> /* malloc */
#include <sys/types.h>
#include <pwd.h>

#include "kdbbackend.h"

#include "datatype.h"
#include "message.h"

#define	HANDLE_PREALLOC	5

static int storeHandle(KDBHandle *handle);
static int delHandle(int kdbdHandle);
static KDBHandle *getHandle(int kdbdHandle);

static	KDBHandle **handles = NULL;		/* Store KDBHandle opened by client */
static	int handleArraySize = 0;

static int storeHandle(KDBHandle *handle)
{
	KDBHandle	**tmp;
	int		kdbdHandle;
	int		newSize;

	newSize = 0;
	if ( handleArraySize == 0 ) {
		newSize = HANDLE_PREALLOC;
	} else {
		/* Search for a free space ... */
		for(kdbdHandle = 0 ; kdbdHandle < handleArraySize ; kdbdHandle++) {
			if ( handles[kdbdHandle] == NULL )
				break;
		}

		if ( kdbdHandle == handleArraySize ) {
			/* No free space found, extend array */
			newSize = handleArraySize + HANDLE_PREALLOC;
		}
	}

	if ( newSize ) {
		/* No free space found, extend array */
		tmp = realloc(handles, sizeof(KDBHandle *) * (newSize));
		if ( tmp == NULL )
			return -1;
		handles = tmp;
		kdbdHandle = handleArraySize;
		handleArraySize = newSize;
	}

	handles[kdbdHandle] = handle;

	return kdbdHandle;
}

static int delHandle(int kdbdHandle)
{
	if ( kdbdHandle >= handleArraySize )
		return -1;

	handles[kdbdHandle] = NULL;

	return 0;
}

static KDBHandle *getHandle(int kdbdHandle)
{
	if ( kdbdHandle >= handleArraySize )
		return NULL;

	return handles[kdbdHandle];
}

Message *wrapper_kdbOpen(Message *request, uid_t euid, gid_t egid)
{
	KDBHandle	*handle;
	struct	passwd	*user;
//	char		*userName;
	int		error, kdbdHandle;
	Message		*reply;
	
	/* Sanity check */
	error = 0;
	if ( messageGetNbArgs(request) != 0 ) 
		return NULL;

	handle = malloc(sizeof(KDBHandle));
	if ( handle == NULL ) {
		perror("wrapper_kdbOpen");
		return NULL;
	}

	kdbOpen(handle);
	error = errno;
	kdbhSetUID(*handle, euid);
	kdbhSetGID(*handle, egid);
	
	kdbdHandle = storeHandle(handle);
	if ( kdbdHandle == -1 ) {
		error = errno;
		kdbClose(handle);
		errno = error;
		return NULL;
	}

	reply = messageNew(MESSAGE_REPLY, KDB_BE_OPEN, 
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_INTEGER, &error,
			DATATYPE_LAST);

	return reply;	
}

Message *wrapper_kdbClose(Message *request)
{
	KDBHandle	*handle;
	int		error, kdbdHandle, ret;
	Message		*reply;

	error = 0;
	if ( messageExtractArgs(request, 
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_LAST) )
		return NULL;
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbClose(handle);
		error = errno;
		reply = messageNew(MESSAGE_REPLY, KDB_BE_CLOSE,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_LAST);
	} else
		reply = NULL;

	return reply;
}

Message *wrapper_kdbStatKey(Message *request)
{
	KDBHandle       *handle;
	Key		*key;
	int             error, ret, kdbdHandle;
	Message		*reply;
	
	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, key,
			DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbStatKey(*handle, key);
		error = errno;
		
		reply = messageNew(MESSAGE_REPLY, KDB_BE_STATKEY,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEY, key,
				DATATYPE_LAST);
	} else
		reply = NULL;
	
	keyDel(key);
	
	return reply;
}

Message *wrapper_kdbGetKey(Message *request)
{
	KDBHandle       *handle;
	Key		*key;
	int		error, ret, kdbdHandle;
	Message		*reply;

	error = 0;
	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
				DATATYPE_INTEGER, &kdbdHandle,
				DATATYPE_KEY, key,
				DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}

	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbGetKey(*handle, key);
		error = errno;

		reply = messageNew(MESSAGE_REPLY, KDB_BE_GETKEY,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEY, key,
				DATATYPE_LAST);
	} else
		reply = NULL;
	
	keyDel(key);
	
	return reply;
}

Message *wrapper_kdbSetKey(void *request)
{
	KDBHandle       *handle;
	Key             *key;
	int             error, ret, kdbdHandle;
	Message         *reply;
	
	error = 0;
	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, key,
			DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbSetKey(*handle, key);
		error = errno;

		reply = messageNew(MESSAGE_REPLY, KDB_BE_SETKEY,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEY, key,
				DATATYPE_LAST);
	} else
		reply = NULL;
	
	keyDel(key);
	
	return reply;
}

Message *wrapper_kdbSetKeys(void *request)
{
	KDBHandle       *handle;
	KeySet		*ks;
	int             error, ret, kdbdHandle;
	Message         *reply;
	
	if ( (ks = ksNew()) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request, 
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST) ) {
		ksDel(ks);
		return NULL;
	}

	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbSetKeys(*handle, ks);
		error = errno;
		reply = messageNew(MESSAGE_REPLY, KDB_BE_SETKEYS,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEYSET, ks,
				DATATYPE_LAST);
	} else
		reply = NULL;

	ksDel(ks);

	return reply;
}

Message *wrapper_kdbRename(void *request)
{
	KDBHandle       *handle;
	Key             *key;
	char		*newKeyName;
	int             error, ret, kdbdHandle;
	Message         *reply;

	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, key,
			DATATYPE_STRING, &newKeyName,
			DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}

	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbRename(*handle, key, newKeyName);
		error = errno;
		free(newKeyName);
	} else
		reply = NULL;

	reply = messageNew(MESSAGE_REPLY, KDB_BE_RENAME,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_LAST);
	keyDel(key);

	return reply;
}

Message *wrapper_kdbRemoveKey(Message *request)
{
	KDBHandle       *handle;
	Key             *key;
	int             error, ret, kdbdHandle;
	Message         *reply;
	
	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, key,
			DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbRemoveKey(*handle, key);
		error = errno;
	
		reply = messageNew(MESSAGE_REPLY, KDB_BE_REMOVEKEY,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_LAST);
	} else
		reply = NULL;
	
	return reply;
}

Message *wrapper_kdbGetChild(Message *request)
{
	KDBHandle       *handle;
	Key             *parentKey;
	KeySet		*ks;
	int             error, ret, kdbdHandle;
	unsigned long	options;
	Message		*reply;
	
	if ( (parentKey = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;

	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, parentKey,
			DATATYPE_ULONG, &options,
			DATATYPE_LAST) ) {
		keyDel(parentKey);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ks = ksNew();
		ret = kdbGetKeyChildKeys(*handle, parentKey, ks, options);
		error = errno;

		reply = messageNew(MESSAGE_REPLY, KDB_BE_GETCHILD,
				DATATYPE_INTEGER, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEYSET, ks,
				DATATYPE_LAST);
		ksDel(ks);
	} else 
		reply = NULL;
	
	keyDel(parentKey);

	return reply;
}

Message *wrapper_kdbMonitorKey(Message *request)
{
	KDBHandle       *handle;
	Key		*key;
	int             error, kdbdHandle;
	unsigned long   diffMask, iterations, sleep, ret;
	Message         *reply;
	
	if ( (key = keyNew(KEY_SWITCH_END)) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEY, key,
			DATATYPE_ULONG, &diffMask,
			DATATYPE_ULONG, &iterations,
			DATATYPE_ULONG, &sleep,
			DATATYPE_LAST) ) {
		keyDel(key);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbMonitorKey(*handle, key, diffMask, iterations, sleep);
		error = errno;
		
		reply = messageNew(MESSAGE_REPLY, KDB_BE_MONITORKEY,
				DATATYPE_ULONG, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEY, key,
				DATATYPE_LAST);
	} else {
		reply = NULL;
	}
	
	keyDel(key);
	
	return reply;
}

Message *wrapper_kdbMonitorKeys(Message *request)
{
	KDBHandle	*handle;
	KeySet		*ks;
	int             kdbdHandle, error;
	unsigned long	diffMask, iterations, sleep, ret;
	Message         *reply;
	
	if ( (ks = ksNew()) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request,
			DATATYPE_INTEGER, &kdbdHandle,
			DATATYPE_KEYSET, ks,
			DATATYPE_ULONG, &diffMask,
			DATATYPE_ULONG, &iterations,
			DATATYPE_ULONG, &sleep,
			DATATYPE_LAST) ) {
		ksDel(ks);
		return NULL;
	}
	
	if ( (handle = getHandle(kdbdHandle)) ) {
		ret = kdbMonitorKeys(*handle, ks, diffMask, iterations, sleep);
		error = errno;
		
		reply = messageNew(MESSAGE_REPLY, KDB_BE_MONITORKEYS,
				DATATYPE_ULONG, &ret,
				DATATYPE_INTEGER, &error,
				DATATYPE_KEYSET, ks,
				DATATYPE_LAST);
	} else {
		reply = NULL;
	}
	
	ksDel(ks);
	
	return reply;	
	
}

