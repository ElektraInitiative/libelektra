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

$Id$

*/


#ifndef DATATYPE_H
#define DATATYPE_H


typedef enum {
	DATATYPE_UNKNOW = 1,
	DATATYPE_INTEGER,
	DATATYPE_STRING,
	DATATYPE_KEY,			/* Complex data */
	DATATYPE_KEYSET,
	DATATYPE_MESSAGE,

	DATATYPE_LAST			/* Must be the last */
} DataType;


#endif /* DATATYPE_H */
