/***************************************************************************
                datatype.h  -  Defines argument datatypes
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

$Id: datatype.h 673 2006-03-13 00:28:19Z aviram $

*/


#ifndef DATATYPE_H
#define DATATYPE_H

/**
 * Arguments type
 *
 * Type accepted in @link message Message@endlink
 *
 * @ingroup message
 * @see messageNew()
 * @see messageExtractArgs()
 */
typedef enum {
	DATATYPE_UNKNOW = 1,		/*!< Unknown, not initialized */
	DATATYPE_INTEGER,		/*!< "int" C type */
	DATATYPE_ULONG,			/*!< "unsigned long" C type */
	DATATYPE_STRING,		/*!< "char *" C type */
	DATATYPE_KEY,			/*!< libelektra Key */
	DATATYPE_KEYSET,		/*!< libelektra KeySet */

	DATATYPE_LAST			/*!< Must be the last */
} DataType;


#endif /* DATATYPE_H */
