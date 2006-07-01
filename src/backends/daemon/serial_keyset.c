/***************************************************************************
                serial_keyset.c  -  Low level objects serialization etc
                             -------------------
    begin                : Sun Mar 12 2006
    copyright            : (C) 2006 by Yannick Lecaillez, Avi Alkalay
    email                : sizon5@gmail.com, avi@unix.sh
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



#include <stdlib.h>
#include <string.h>

#include "kdb.h"
#include "kdbprivate.h"

#include "datatype.h"
#include "message.h"

#include "serial_key.h"
#include "serial_keyset.h"

ssize_t serialKeySet_getSize(const void *pKeySet) 
{
	const KeySet *ks;
	size_t	size;
	ssize_t ret;
	Key	*current;
	
	ks = (const KeySet *) pKeySet;
	size = sizeof(ks->size);
	current = ks->start;
	while ( current != NULL ) {
		ret = serialKey_getSize((const void *) current);
		if ( ret == -1 )
			return -1;		
		
		size += ret;
		current = current->next;
	}
	
	size += sizeof(size_t);
	
	return size;
}

ssize_t serialKeySet_unserialize(const void *pBuffer, void *pKeySet)
{
	KeySet	*ks;
	const char *buf;
	ssize_t	ret;
	size_t	count, size;
	Key	*current;

	buf = (const char *) pBuffer;
	ks = (KeySet *) pKeySet;
	
	/* Read size of the keySet */
	memcpy(&size, buf, sizeof(size));
	buf += sizeof(size);

	/* Read the key inside the keyset */
	count = 0;
	while ( count < size ) {
		/* Create a new key */
		current = keyNew(KEY_SWITCH_END);
		if ( current == NULL )
			return -1;

		/* Unserialize key */
		ret = serialKey_unserialize(buf, (void *) current);
		if ( ret == -1 )
			return -1;
		buf += ret;

		/* Add the key into the keyset */
		count = ksAppend(ks, current);
	}
	
	return (buf - ((const char *) pBuffer));
}

ssize_t serialKeySet_serialize(const void *pKeySet, void *pBuffer) 
{
	const KeySet *ks;
	Key *current;
	char *buf;
	ssize_t ret;
	
	ks = (const KeySet *) pKeySet;
	buf = pBuffer;
	
	memcpy(buf, &ks->size, sizeof(ks->size));
	buf += sizeof(ks->size);

	/* Store all the keyset */
	current = ks->start;
	while ( current != NULL ) {
		ret = serialKey_serialize((void *) current, (void *) buf);
		if ( ret == -1 )
			return -1;
		buf += ret;

		current = current->next;
	}

	return (buf - ((char *) pBuffer));
}
