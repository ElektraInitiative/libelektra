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

ssize_t serializeGetSize(DataType type, void *val)
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST )
		return -1;

	return serializer[type].getSizeOfSerialized(val);
}


ssize_t serialize(DataType type, const void *pType, void *pBuffer)
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST )
		return -1;
	
	return serializer[type].serialize(pType, pBuffer);
}

ssize_t unserialize(DataType type, const void *pBuffer, void *pType) 
{
	if ( type <= DATATYPE_UNKNOW || type >= DATATYPE_LAST )
		return -1;
	
	return serializer[type].unserialize(pBuffer, pType);
}
