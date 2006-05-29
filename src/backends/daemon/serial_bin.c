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



#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "kdb.h"
#include "kdbprivate.h"

#include "datatype.h"
#include "argument.h"
#include "message.h"

#include "serial_bin.h"

size_t messageSerializeGetSize(const Message *msg) {
	const	Argument	*arg;
		size_t		size;
		int		i;

	printf("messageSerializeGetSize()\n");
		
	assert(msg != NULL);

	size = sizeof(Message);
	for(i = 0 ; i<msg->nbArgs ; i++) {
		arg = messageStealArgByIndex(msg, i);
		size += argumentSerializeGetSize(arg);
	}

	return size;
}

ssize_t messageSerialize(const Message *msg, void *data, size_t dataLen) {
	ssize_t	ret;
	size_t	size, rest, offset;
	int	i;

	printf("messageSerialize()\n");
	
	assert(msg != NULL);
	assert(data != NULL);

	offset = 0;
	rest = dataLen;

	/* Serialize message struct */
	size = sizeof(Message);
	if ( rest < size ) {
		errno = KDB_RET_TRUNC;
		return -1;
	}
	memcpy(data, msg, size);
	offset += size;
	rest -= size;

	/* Serialize message arguments */
	for(i = 0 ; i < msg->nbArgs ; i++) {
		ret = argumentSerialize(msg->args[i], data + offset, rest);
		if ( ret == -1 )
			return -1;
		offset += ret;
		rest -= ret;
	}

	return offset;
}



ssize_t messageUnserialize(const void *data, Message *msg) {
	Argument *arg;
	int      i, nbArgs;
	ssize_t  ret;
	size_t   size;
	size_t   offset = 0;
	
	printf("messageUnserialize()\n");
	
	/* Unserialize message */
	size = sizeof(Message);
	memcpy(msg, data, size);
	offset += size;

	/* This message doesn't contain args yet
	 * they will be added later */

	printf("\tI found %d args\n", msg->nbArgs);
	
	msg->args = NULL;
	nbArgs = msg->nbArgs;
	msg->nbArgs = 0;

	/* Unserialize message args */ 
	for(i = 0 ; i < nbArgs ; i++) {
		if ( (arg = argumentNew()) == NULL ) {
			printf("**error new\n");
			return -1;
		}
		
		ret = argumentUnserialize(data + offset, arg);
		if ( ret == -1 ) {
			argumentDel(arg);
			return -1;
		}
		offset += ret;

		if ( messageAddArgument(msg, arg) == -1 ) {
			argumentDel(arg);
			return -1;
		}
	}

	printf("nbArgs set to = %d\n", msg->nbArgs);
	
	return offset;
}


size_t argumentSerializeGetSize(const Argument *arg) {
	size_t	size;
	
	assert(arg != NULL);

	printf("argumentSerializeGetSize()\n");
	
	size = sizeof(Argument);
	switch ( arg->type ) {
		case DATATYPE_STRING:
			size += strlen(arg->data.string) + 1;
			break;
		case DATATYPE_KEY:
			size += keySerializeGetSize((Key *) arg->data.complexData);
			break;
		case DATATYPE_KEYSET:
			size += keySetSerializeGetSize((KeySet *) arg->data.complexData);
			break;
	}

	return size;
}

ssize_t argumentSerialize(const Argument *arg, void *data, size_t dataLen) {
	size_t	offset, rest;
	ssize_t	size;
	
	assert(data != NULL);

	printf("argumentSerialize()\n");
	
	offset = 0;
	rest = dataLen;
	
	/* Copy the message arg struct */
	size = sizeof(Argument);
	if ( rest < size ) {
		errno = KDB_RET_TRUNC;
		return -1;
	}
	memcpy(data, arg, size);
	rest -= size;
	offset += size; 

	/* Copy complex data if needed */
	switch(arg->type) {
		case DATATYPE_STRING:
			size = strlen(arg->data.string) + 1;
			if ( rest < size ) {
				errno = KDB_RET_TRUNC;
				return -1;
			}
			memcpy(data + offset, arg->data.string, size);
			rest -= size;
			offset += size;
			break;
			
		case DATATYPE_KEY:
			size = keySerialize((const Key *) arg->data.complexData, data + offset, rest);
			if ( size == -1 ) {
				errno = KDB_RET_TRUNC;
				return -1;
			}
			rest -= size;
			offset += size;
			break;

		case DATATYPE_KEYSET:
			size = keySerialize((const Key *) arg->data.complexData, data + offset, rest);
			if ( size == -1 ) {
				errno = KDB_RET_TRUNC;
				return -1;
			}
                        rest -= size;
			offset += size;
			break;
	}

	return offset;
}






ssize_t argumentUnserialize(const void *data, Argument *arg) {
	size_t  size=0;
	size_t  offset = 0;
	Key     *key=0;
	KeySet  *ks=0;
	
	assert(data != NULL);
	assert(arg != NULL);

	printf("argumentUnserialize()\n");
	
	/* Unserialize argument struct */
	memcpy(&(arg->type),data,size=sizeof(DataType));
	offset += size;

	/* Unserialize complex data */
	switch(arg->type) {
		case DATATYPE_INTEGER:
			size = sizeof(int);
			arg->data.integer=*(int *)(data+offset);
			offset+=size;
			break;

		case DATATYPE_STRING:
			size = strblen(data+offset);
			arg->data.string = (char *) malloc(size);
			if ( arg->data.string == NULL )
				return -1;
			memcpy(arg->data.string, data + offset, size);
			offset += size;
			break;
			
		case DATATYPE_KEY:
			key = keyNew(KEY_SWITCH_END);
			size = keyUnserialize(data + offset, key);
			if ( size == -1 )
				return -1;
			arg->data.complexData = key;
			offset += size;
			break;
			
		case DATATYPE_KEYSET:
			printf("TODO\n");
			break;
	}
	
	printf("\t\tOFFSET = %ld\n", offset);
	
	return offset;
}


/**
 * Get serialized size of a key
 *
 * @param key Key
 *
 * @return # bytes of the key's serialized form
 *
 * @see keySerialize(), keyUnserialize()
 */
size_t keySerializeGetSize(const Key *key) {
	size_t	size;
	
	printf("keySerializeGetSize()\n");
	
	if ( key == NULL ) {
		errno = KDB_RET_NULLKEY;
		return -1;
	} else if ( !keyIsInitialized(key) ) {
		errno = KDB_RET_UNINITIALIZED;
		return -1;
	}

	size  = sizeof(Key);
	if ( key->flags & KEY_SWITCH_NAME )
		size += keyGetNameSize(key);
        if ( key->flags & KEY_SWITCH_COMMENT )
		size += keyGetCommentSize(key);
	if ( key->flags & KEY_SWITCH_OWNER )
		size += keyGetOwnerSize(key);
	if ( key->flags & KEY_SWITCH_VALUE )
		size += keyGetValueSize(key);

	return size;
}

/**
 * Get serialized size of a keyset 
 *
 * @param ks KeySet
 *
 * @return Size of the keyset's serialied form
 * or -1 if error
 *
 * @see keySetBinSerialize(), keySetBinUnserialize()
 */
size_t keySetSerializeGetSize(KeySet *ks) {
	size_t	size, ret;
	Key	*init;
	Key	*current;
	
	assert(ks != NULL);

	printf("keySetSerializeGetSize()\n");
	
	size = sizeof(ks->size);
	
	init = ksCurrent(ks);
	while ( (current = ksNext(ks)) != NULL ) {
		if ( (ret = keySerializeGetSize(current)) == -1 ) {
			ks->cursor = init;
			return -1;
		}

		size += ret;
	}
	ks->cursor = init;
	
	size += sizeof(size_t);
	
	return size;
}

/**
 * Serialize a key struct
 *
 * The key is serialized accordingly to its flags. That mean
 * if key haven't got name, name will not be serialized.
 *
 * @param key Initialized Key struct to serialize
 * @param data Where to put the serialized key data
 * @param dataSize Size of the data buffer
 * 
 * @return # bytes copied into data (-1 if error)
 *
 * @see keyUnserialize()
 */
ssize_t keySerialize(const Key *key, void *data, size_t dataSize) {
	size_t	size, copied, rest;

	printf("keySerialize()\n");
	
	assert(data != NULL);
	
	if (key == NULL) {
		errno = KDB_RET_NULLKEY;
		return -1;
	} else if ( !keyIsInitialized(key) ) {
		errno = KDB_RET_UNINITIALIZED;
		return -1;
	}

	rest = dataSize;
	
	/* Serialize metadata */
	size = sizeof(Key);
	if ( rest < size ) {
		errno = KDB_RET_TRUNC;
		return -1;
	}
	memcpy(data, key, size);
	copied = size;
	rest -= size;

	/* Serialize key name */
	if ( key->flags & KEY_SWITCH_NAME ) {
		size = keyGetNameSize(key);
		if ( rest < size ) {
			errno = KDB_RET_TRUNC;
			return -1;
		}
		memcpy(data + copied, keyStealName(key), size);
		copied += size;
		rest -= size;
	}

	/* Serialize key comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		size = keyGetCommentSize(key);
		if ( rest < size ) {
			errno = KDB_RET_TRUNC;
			return -1;
		}	
		memcpy(data + copied, keyStealComment(key), size);
		copied += size;
		rest -= size;
	}

	/* Serialize key owner */
	if ( key->flags & KEY_SWITCH_OWNER ) {
		size = keyGetOwnerSize(key);
		if ( rest < size ) {
			errno = KDB_RET_TRUNC;
			return -1;
		}	
		memcpy(data + copied, keyStealOwner(key), size);
		copied += size;
		rest -= size;	
	}
	
	/* Serialize key value */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		size = keyGetValueSize(key);
		if ( rest < size ) {
			errno = KDB_RET_TRUNC;
			return -1;
		}	
		memcpy(data + copied, keyStealValue(key), size);
		copied += size;
		rest -= size;
	}

	printf("keySerialize() = %ld\n", copied);
	
	return copied;
}

/**
 * Unserialize a key
 * 
 * Read data from @p data and fill @p key accordingly.
 * 
 * @param data Serialized data of a key
 * @param key Initialized key struct to fill with data
 *
 * @return # bytes read from data for unserialize key (-1 if error) 
 */ 
ssize_t keyUnserialize(const void *data, Key *key) {
	const	char	*tmp;
		size_t	offset;
		size_t	size;

	assert(data != NULL);

	printf("keyUnserialize()\n");
	
	if ( key == NULL ) {
		errno = KDB_RET_NULLKEY;
		printf("ERROR: keyUnserialize\n");
		return -1;
	} else if ( !keyIsInitialized(key) ) {
		errno = KDB_RET_UNINITIALIZED;
		printf("ERROR: keyUnserialize\n");
		return -1;
	}

	tmp = data;
	
	/* Unserialize metadata */
	size = sizeof(Key);
	memcpy(key, data, size);
	offset = size;
	tmp += size;

	/* Unserialize keyname */
	if ( key->flags & KEY_SWITCH_NAME ) {
		size = strblen(tmp);
		
		if ( (key->key = (char *) malloc(size)) == NULL ) {
			printf("ERROR: malloc\n");
			return -1;
		}
		memcpy(key->key, tmp, size);
		
		offset += size;
		tmp += size;
	}

	/* Unserialize comment */
	if ( key->flags & KEY_SWITCH_COMMENT ) {
		size = strblen(tmp);
		
		if ( (key->comment = (char *) malloc(size)) == NULL ) {
                        printf("ERROR: malloc\n");
			free(key->key);
			return -1;
		}
		memcpy(key->comment, tmp, size);
		
		offset += size;
		tmp += size;
	}

	/* Unserialize userDomain */
	if ( key->flags & KEY_SWITCH_OWNER ) {
		size = strblen(tmp);
		
		if ( (key->userDomain = (char *) malloc(size)) == NULL ) {
			printf("ERROR: malloc\n");
			free(key->key);
			free(key->comment);
			return -1;
		}
		memcpy(key->userDomain, tmp, size);
		
		offset += size;
		tmp += size;
	}

	/* Unserialize data */
	if ( key->flags & KEY_SWITCH_VALUE ) {
		size = key->dataSize;
	
		if ( (key->data = malloc(size)) == NULL ) {
                        printf("ERROR: malloc\n");
			free(key->key);
			free(key->comment);
			free(key->userDomain);
			return -1;
		}
	
		memcpy(key->data, tmp, size);

		offset += size;
		tmp += size;
	}

	printf("** OK %ld **\n", offset);
	
	return offset;
}

/**
 * Unserialize a keyset
 *
 * Read keyset data from @p data and rebuild the entire KeySet
 * Keys inside the KeySet are created. If @p returnded contain
 * keys yet, new Key are appended at the end and the cursor is
 * changed.
 *
 * @param data Where to read data from
 * @param returned KeySet struct to fill
 *
 * @return #bytes read from data for unserialize the keyset
 * 
 */
ssize_t keySetUnserialize(const void *data, KeySet *returned) {
	Key	**tab;
	size_t	offset, count, ret, cursor;
	Key	*current;

	printf("keySetUnserialize()\n");
	
	assert(data != NULL);
	assert(returned != NULL);

	offset = 0;

	/* Read size of the keySet */
	memcpy(&returned->size, data, sizeof(returned->size));
	offset += 4;

	/* Allocate tab for keep all key pointer */
	if ( (tab = (Key **) calloc(returned->size, sizeof(Key *))) == NULL ) {
		perror("calloc");
		return -1;
	}
	memset(tab, 0, sizeof(Key *) * returned->size);
	
	/* Read the key inside the keyset */
	count = 0;
	while ( count < returned->size ) {
		/* Create a new key */
		if ( (current = keyNew(KEY_SWITCH_END)) == NULL ) {
			for(; count != 0 ; count--) 
				keyDel(tab[count]);
			free(tab);
			perror("keyNew");
			return -1;
		}
		keyInit(current);

		/* Unserialize key */
		if ( (ret = keyUnserialize(data + offset, current)) == -1 ) {
			for(; count != 0 ; count--)
				keyDel(tab[count]);
			free(tab);
			return -1;
		}
		offset += ret;

		/* Add the key into the keyset */
		tab[count] = current;
		count = ksAppend(returned, current);	
	}
	assert(count == returned->size);
	
	returned->end = current;
	
	/* Read the cursor pos */
	memcpy(&cursor, data + offset, sizeof(cursor));
	offset += sizeof(cursor); 
	returned->cursor = tab[cursor];
	
	free(tab);
	
	return offset;
}

/**
 * Serialize a keyset
 * 
 * @param ks Where to read data from
 * @param data Where to write data to
 * @param dataLen Size of the data buffer
 * 
 * @return #bytes written into data (-1 if error)
 *
 * @see keySetBinSerializeGetSize(), keySetBinUnserialize()
 */
ssize_t keySetSerialize(KeySet *ks, void *data, size_t dataLen) {
	Key	*init;
	Key	*current;
	size_t	offset, rest, ret;
	size_t	count, cursor;
	
	assert(ks != NULL);
	assert(data != NULL);

	printf("ketSetSerialize()\n");
	
	offset = 0;

	/* Store the size of the KeySet */
	if ( dataLen < sizeof(ks->size) ) {
		errno = KDB_RET_TRUNC;
		return -1;
	}
	memcpy(data + offset, &ks->size, sizeof(ks->size));
	offset += sizeof(ks->size);	

	/* Store all the keyset */
	init = ks->cursor;
	ksRewind(ks);
	cursor = count = 0;
	rest = dataLen;
	while ( (current = ksNext(ks)) != NULL ) {
		/* Serialize current key */
		if ( (ret = keySerialize(current, data + offset, rest)) == -1 ) {
			ks->cursor = init;
			return -1;
		}
		
		/* The current key is the one pointed by the cursor */
		if ( current == init ) 
			cursor = count;
		
		offset += ret;
		rest -= ret;
		count++;
	}

	assert(count == ks->size);
	
	/* Store the cursor pos */
	memcpy(data + offset, &cursor, sizeof(cursor)); 
	offset += sizeof(cursor);

	ks->cursor = init;
	
	return offset;
}

