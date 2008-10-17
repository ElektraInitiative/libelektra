/***************************************************************************
                 serialze.c  -  Serializing Methods
                             -------------------
    begin                : Sat Nov 23 2007
    copyright            : (C) 2007 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"


/**
 * Returns the size of a key.
 *
 * This is the space needed for metainfo,
 * data, comment, name and owner.
 *
 * @param key the key object to work with
 */
size_t keyGetSerializedSize(const Key *key) {
	return sizeof (Key) +
		key->dataSize +
		key->commentSize +
		key->keySize +
		key->ownerSize;
}


/**
 * Writes a block of memory with the entire key serialized, including
 * metainfo, value, comment and full name.
 *
 * @param key the key object to work with
 * @param serialized where to store the serialized data
 * @param maxSize size in buffer reserved for serialized data
 * @return -1 on failure
 * @return 0 on success
 * @see keyUnserialize()
 * @ingroup keymisc
 */
int keySerialize(const Key *key, void *serialized, size_t maxSize)
{
	size_t recordSize=0;

	recordSize=keyGetSerializedSize(key);

	if (recordSize > maxSize)
	{
		/*errno = KDB_ERR_TRUNC;*/
		return -1;
	}

	memset(serialized,0,recordSize);

	/* First part: the struct */
	memcpy(serialized,key,sizeof(Key));

	/* Second part: the comment */
	if (key->comment) memcpy((char *)(serialized)+sizeof(Key),
			key->comment,key->commentSize);

	/* Third part: the value */
	if (key->data) memcpy((char *)(serialized)+sizeof(Key)+key->commentSize,
			key->data,key->dataSize);

	/* Fourth part: the name */
	if (key->key) memcpy((char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize,
			key->key,key->keySize);

	/* Fifth part: the owner */
	if (key->owner) memcpy((char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize+key->keySize,
			key->owner,key->ownerSize);

	return recordSize;
}

/**
 * Given a membory block created by keySerialize(), unserialize it into
 * a Key structure and return it
 *
 * The @p serialized can be freed after this call, because memory will be
 * allocated for all elements of the new key;
 *
 * keyDel() the key.
 *
 * @param serialized points to memory block where serialized key resides
 * @see keySerialize()
 * @ingroup keymisc
 */
Key *keyUnserialize(const void *serialized) {
	Key *key=0;
	
	if (!serialized) return 0;
	
	key=malloc(sizeof(Key));
	
	/* First part: the struct */
	memcpy(key,serialized,sizeof(Key));
	
	/* Second part: the comment */
	if (key->commentSize) {
		key->comment=malloc(key->commentSize);
		memcpy(key->comment,
			(char *)(serialized)+sizeof(Key),
			key->commentSize);
	}
	
	/* Third part: the value */
	if (key->dataSize) {
		key->data=malloc(key->dataSize);
		memcpy(key->data,
			(char *)(serialized)+sizeof(Key)+key->commentSize,
			key->dataSize);
	}
	
	/* Fourth part: the key name */
	if (key->keySize) {
		key->key=malloc(key->keySize);
		memcpy(key->key,
			(char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize,
			key->keySize);
	}

	/* Fifth part: the owner */
	if (key->ownerSize) {
		key->owner=malloc(key->ownerSize);
		memcpy(key->owner,
			(char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize+key->keySize,
			key->ownerSize);
	}

	return key;
}

/**
 * Compose a key out of a memoryblock created by keySerialize().
 *
 * No additional memory will be allocated. Make sure not to free or munmap
 * @p serialized.
 *
 * keyDel() actually does not del the key, so it can be saftely be used in keySets.
 * To get rid of the key just delete @p serialized, but be sure that all references
 * are gone: remove it from any keyset and no pointer should be left pointing on
 * the key.
 *
 * @param serialized points to memory block where serialized key resides
 * @see keySerialize()
 * @ingroup keymisc
 */
Key *keyCompose(const void *serialized) {
	Key *key=0;

	if (!serialized) return 0;

	key=(Key *)serialized;

	/* First part: the metainfo */
	if (key->commentSize) key->comment=(char *)(serialized)+sizeof(Key);

	/* Third part: the value */
	if (key->dataSize) key->data= (char *)(serialized)+sizeof(Key)+key->commentSize;

	/* Fourth part: the name */
	if (key->keySize) key->key=(char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize;

	/* Fifth part: the owner*/
	if (key->ownerSize) key->owner=(char *)(serialized)+sizeof(Key)+key->commentSize+key->dataSize+key->keySize;

	key->ksReference = SSIZE_MAX;

	return key;
}

