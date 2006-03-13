/***************************************************************************
                argument.h  -  Class to encapsulte message arguments
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


#ifndef ARGUMENT_H
#define ARGUMENT_H


#include "datatype.h"


/* Struct which store an RPC Argument */
typedef struct {
	DataType type;	/* Type of this args (see ArgType)	*/
	
	union {
		void    *complexData;	/* Complex data (struct) */
		char    *string;	/* String data */
		int     integer;	/* integer data */
	} data;
} Argument;

Argument *argumentNew();
int argumentInit(Argument *arg);
int argumentSetValue(Argument *arg, DataType type, const void *value);
int argumentClose(Argument *arg);
int argumentDel(Argument *arg);

#endif /* ARGUMENT_H */