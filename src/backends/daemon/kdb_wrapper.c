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

$Id$

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* malloc */
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "kdbbackend.h"

#include "datatype.h"
#include "message.h"


Message *wrapper_kdbStatKey(KDB *handle, Message *request)
{
	Key		key;
	int             error, ret;
	Message		*reply;
	
	keyInit(&key);
	if ( messageExtractArgs(request,
			DATATYPE_KEY, &key,
			DATATYPE_LAST) ) {
		keyClose(&key);
		return NULL;
	}
	
	ret = kdbStatKey(handle, &key);
	error = errno;
	
	reply = messageNew(MESSAGE_REPLY, KDB_BE_STATKEY,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_KEY, &key,
			DATATYPE_LAST);
	
	keyClose(&key);
	
	return reply;
}

Message *wrapper_kdbGetKey(KDB *handle, Message *request)
{
	Key		key;
	int		error, ret;
	Message		*reply;

	keyInit(&key);
	error = 0;
	
	if ( messageExtractArgs(request,
				DATATYPE_KEY, &key,
				DATATYPE_LAST) ) {
		keyClose(&key);
		return NULL;
	}

	ret = kdbGetKey(handle, &key);
	error = errno;

	reply = messageNew(MESSAGE_REPLY, KDB_BE_GETKEY,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_KEY, &key,
			DATATYPE_LAST);

	keyClose(&key);
	
	return reply;
}

Message *wrapper_kdbSetKey(KDB *handle, void *request)
{
	Key             key;
	int             error, ret;
	Message         *reply;
	
	keyInit(&key);
	error = 0;
	
	if ( messageExtractArgs(request,
			DATATYPE_KEY, &key,
			DATATYPE_LAST) ) {
		keyClose(&key);
		return NULL;
	}

	ret = kdbSetKey(handle, &key);
	error = errno;
	
	reply = messageNew(MESSAGE_REPLY, KDB_BE_SETKEY,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_KEY, &key,
			DATATYPE_LAST);
	
	keyClose(&key);
	
	return reply;
}

Message *wrapper_kdbSetKeys(KDB *handle, void *request)
{
	KeySet		*ks;
	int             error, ret;
	Message         *reply;
	
	if ( (ks = ksNew()) == NULL )
		return NULL;
	
	if ( messageExtractArgs(request, 
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST) ) {
		ksDel(ks);
		return NULL;
	}

	ret = kdbSetKeys(handle, ks);
	error = errno;
	reply = messageNew(MESSAGE_REPLY, KDB_BE_SETKEYS,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST);

	ksDel(ks);

	return reply;
}

Message *wrapper_kdbGetChild(KDB *handle, Message *request)
{
	Key             parentKey;
	KeySet		*ks;
	int             error, ret;
	unsigned long	options;
	Message		*reply;
	
	keyInit(&parentKey);

	if ( messageExtractArgs(request,
			DATATYPE_KEY, &parentKey,
			DATATYPE_ULONG, &options,
			DATATYPE_LAST) ) {
		keyClose(&parentKey);
		return NULL;
	}
	
	ks = ksNew();
	ret = kdbGetKeyChildKeys(handle, &parentKey, ks, options);
	error = errno;
	keyClose(&parentKey);
	
	reply = messageNew(MESSAGE_REPLY, KDB_BE_GETCHILD,
			DATATYPE_INTEGER, &ret,
			DATATYPE_INTEGER, &error,
			DATATYPE_KEYSET, ks,
			DATATYPE_LAST);
	ksDel(ks);

	return reply;
}
