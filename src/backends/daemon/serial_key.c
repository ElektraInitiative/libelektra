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

$Id: serial_bin.c 788 2006-05-29 16:30:00Z aviram $

*/



#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "kdb.h"
#include "kdbprivate.h"

#include "datatype.h"
#include "message.h"

#include "serial_key.h"

ssize_t serialKey_getSize(const void *pKey)
{
	const Key *key;
	size_t	size;

	key = (const Key *) pKey;
		
	if ( !keyIsInitialized(key) ) 
		return -1;

	size  = sizeof(Key);
	if ( key->flags & KEY_SWITCH_NAME )
		size += keyGetNameSize(key);
        if ( key->flags & KEY_SWITCH_COMMENT )
		size += keyGetCommentSize(key);
	if ( key->flags & KEY_SWITCH_DOMAIN )
		size += keyGetOwnerSize(key);
	if ( key->flags & KEY_SWITCH_VALUE )
		size += keyGetValueSize(key);
	
	return size;
}

ssize_t serialKey_serialize(const void *pKey, void *pBuffer)
{
	const Key *key;
	size_t	size;
	char	*buf;

	key = (const Key *) pKey;
	
	if ( !keyIsInitialized(key) ) {
		return -1;
	}
	buf = (char *) pBuffer;		

	/* Serialize metadata */
	size = sizeof(Key);
	memcpy(buf, key, size);
	buf += size;

	/* Serialize key owner */
	if ( key->flags & KEY_SWITCH_DOMAIN ) {
		size = keyGetOwnerSize(key);
		memcpy(buf, keyStealOwner(key), size);
		buf += size;
	}
	
	/* Serialize key name */
	if ( key->flags & KEY_SWITCH_NAME ) {
		size = keyGetNameSize(key);
		memcpy(buf, keyStealName(key), size);
		buf += size;
	}

	/* Serialize key comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		size = keyGetCommentSize(key);
		memcpy(buf, keyStealComment(key), size);
		buf += size;
	}

	/* Serialize key value */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		size = keyGetValueSize(key);
		memcpy(buf, keyStealValue(key), size);
		buf += size;
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
	memset(key, 0, sizeof(Key));
	
	/* Unserialize key struct */
	size = sizeof(Key);
	memcpy(key, buf, size);
	buf += size;

	/* Restore original key pointer */
	key->key = keyStealName(&save);
	key->comment = keyStealComment(&save);
	key->userDomain = keyStealOwner(&save);
	key->data = keyStealValue(&save);

	/* Unserialize userDomain */
	if ( key->flags & KEY_SWITCH_DOMAIN ) {
		size = keySetOwner(key, buf);
		buf += size;
	}
		
	
	/* Unserialize keyname */
	if ( key->flags & KEY_SWITCH_NAME ) {
		size = keySetName(key, buf);
		buf += size;
	}

	/* Unserialize comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		size = keySetComment(key, buf);
		buf += size;
	}

	/* Unserialize data */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		size = keySetRaw(key, buf, key->dataSize);
		buf += size;
	}

	return (buf - ((char *) pBuffer));
}
