/***************************************************************************
                serial_bin.c  -  Low level objects serialization etc
                             -------------------
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

$Id: serial_bin.c 788 2006-05-29 16:30:00Z aviram $

*/

#include "datatype.h"

#include "serial.h"
#include "serial_int.h"
#include "serial_string.h"
#include "serial_key.h"
#include "serial_keyset.h"

/* *BEWARE* These funcs must be ordered by datatype in a such way
 * serialize[DATATYPE] return expected results.
 * Have a look at datatype.h
 */
static SerializerFunc serializer[] = {
	{ NULL, NULL, NULL }, 		/*	 [0] -> Uninitialized	*/
	{ NULL, NULL, NULL },		/*	 [1] -> Unknown		*/

	/* Integer */
	{ serialInt_getSize, serialInt_serialize, serialInt_unserialize },
	
	/* Unsigned Long */
	{ serialUlong_getSize, serialUlong_serialize, serialUlong_unserialize },
	
	/* String */
	{ serialString_getSize, serialString_serialize, serialString_unserialize },

	/* Key */	
	{ serialKey_getSize, serialKey_serialize, serialKey_unserialize },
	
	/* KeySet */
	{ serialKeySet_getSize, serialKeySet_serialize, serialKeySet_unserialize }
};

/**
 * @defgroup serializer Serializer
 * @brief Serializer provide methods for serialize/unserialize data of several DataType
 *
 * Serializer is used by @link message Message@endlink module for arguments
 * serialization/unserialization.
 * 
 * These functions are wrapper to specific methods depending of the DataType of data
 * they're working on. Each DataType have its own function set for do the real work.
 *
 */

/**
 * Get size of a serialized arguments
 *
 * This function compute the size needed for store
 * the serialized form of an argument of type "type"
 * and of value "val"
 *
 * @param type Type of the argument
 * @param val Value of the argument
 *
 * @return Size of the serialized or -1 if error
 *
 * @see DataType
 * @ingroup serializer
 */
ssize_t serializeGetSize(DataType type, void *val)
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST ) {
		errno = ERANGE;
		return -1;
	}

	return serializer[type].getSizeOfSerialized(val);
}


/**
 * Serialize argument
 *
 * Serialize an argument into a buffer
 
 * @param type Type of the argument to serialize
 * @param pType Pointer to the argument
 * @param pBuffer Buffer which will receive serialized version
 *
 * @return Size of the serialized or -1 if error
 *
 * @see unserialize()
 * @ingroup serializer
 */
ssize_t serialize(DataType type, const void *pType, void *pBuffer)
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST ) {
		errno = ERANGE;
		return -1;
	}
	
	return serializer[type].serialize(pType, pBuffer);
}


/**
 * Unserialize argument
 *
 * Unserialize data from a buffer to "rebuild" argument
 *
 * @param type Type of the argument to unserialize
 * @param pBuffer Buffer containing serialized version of argument
 * @param pType Pointer to the unserialized argument
 * *WARNING*: If argument's type have a flexible size (not like a 
 * struct containing pointer but like a string) passed pType should
 * be a **pType. Unserialized will then allocate needed memory for this
 * type. You'll have to freed this one
 *
 * @return Size of the serialized or -1 if error
 * @see serialize()
 * @ingroup serializer
 */
ssize_t unserialize(DataType type, const void *pBuffer, void *pType) 
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST ) {
		errno = ERANGE;
		return -1;
	}
	
	return serializer[type].unserialize(pBuffer, pType);
}
