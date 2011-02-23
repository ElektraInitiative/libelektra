/***************************************************************************
                serial_bin.c  -  Low level objects serialization etc
                             -------------------
    begin                : Sun Mar 12 2006
    copyright            : (C) 2006 by Yannick Lecaillez, Avi Alkalay
    email                : avi@unix.sh
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

#include <assert.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"

#include "datatype.h"
#include "message.h"

#include "serial_string.h"
#include "serial_key.h"

ssize_t serialKey_getSize(const void *pKey)
{
	const Key *key;
	size_t	size;

	key = (const Key *) pKey;
		
	if ( !keyIsInitialized(key) ) 
		return -1;

	size = sizeof(Key);
	
	if ( key->flags & KEY_SWITCH_NAME )
		size += serialString_getSize(keyName(key));
        if ( key->flags & KEY_SWITCH_COMMENT )
		size += serialString_getSize(keyComment(key));
	if ( key->flags & KEY_SWITCH_OWNER )
		size += serialString_getSize(keyOwner(key));
	if ( key->flags & KEY_SWITCH_VALUE ) {
		if ( keyIsString(key) )
			size += serialString_getSize(keyValue(key));
		else
			size += keyGetValueSize(key);
	}

	return size;
}

ssize_t serialKey_serialize(const void *pKey, void *pBuffer)
{
	const Key *key;
	size_t	size;
	char	*buf;
	int	convert;

	key = (const Key *) pKey;
	
	if ( !keyIsInitialized(key) ) {
		return -1;
	}
	buf = (char *) pBuffer;		

	/* Serialize metadata */
	size = sizeof(Key);
	memcpy(buf, key, size);
	buf += size;

	convert = kdbbNeedsUTF8Conversion();
	
	/* Serialize key name */
	if ( key->flags & KEY_SWITCH_NAME ) {
		size = serialString_serialize(keyName(key), buf);	
		if ( size == -1 )
			return -1;
		buf += size;
	}

	/* Serialize key comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		size = serialString_serialize(keyComment(key), buf);
		if ( size == -1 )
			return -1;
		buf += size;
	}

	/* Serialize key owner */
	if ( key->flags & KEY_SWITCH_OWNER ) {
		size = serialString_serialize(keyOwner(key), buf);
		if ( size == -1 )
			return -1;
		buf += size; 
	}
	
	/* Serialize key value */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		if ( keyIsString(key) ) {
			size = serialString_serialize(keyValue(key), buf);
			if ( size == -1 )
				return -1;
			buf += size;
		} else {
			size = keyGetValueSize(key);
			memcpy(buf, keyValue(key), size);
			buf += size;
		}
	}

	return (buf - ((char *) pBuffer));
}

ssize_t serialKey_unserialize(const void *pBuffer, void *pKey) 
{
	const	char	*buf;
		Key	*key, save;
		size_t	size;

	key = (Key *) pKey;
	if ( !keyIsInitialized(key) ) 
		return -1;

	buf = (const char *) pBuffer;

	/* Save current key char pointer 
	 * since these one will be overrided. */
	memcpy(&save, key, sizeof(Key));
	
	/* Unserialize key struct */
	size = sizeof(Key);
	memcpy(key, buf, size);
	buf += size;

	/* Restore original key pointer */
	key->key = keyName(&save);
	key->comment = keyComment(&save);
	key->userDomain = keyOwner(&save);
	key->data = keyValue(&save);

	/* Unserialize keyname */
	if ( key->flags & KEY_SWITCH_NAME ) {
		free(key->key);
		size = serialString_unserialize(buf, &key->key);
		if ( size == -1 )
			return -1;
		buf += size;
	}

	/* Unserialize comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		free(key->comment);
		size = serialString_unserialize(buf, &key->comment);
		if ( size == -1 )
			return -1;
		key->commentSize = size;
		buf += size;
	}

	/* Unserialize userDomain */
	if ( key->flags & KEY_SWITCH_OWNER ) {
		free(key->userDomain);
		size = serialString_unserialize(buf, &key->userDomain);
		if ( size == -1 )
			return -1;
		buf += size; 
	}

	/* Unserialize data */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		if ( keyIsString(key) ) {
			free(key->data);
			size = serialString_unserialize(buf, &key->data);
		} else {
			size = keySetRaw(key, buf, key->dataSize);
		}

		if ( size == -1 )
			return -1;

		key->dataSize = size;
		buf += size;
	}

	return (buf - ((char *) pBuffer));
}
