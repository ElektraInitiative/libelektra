/***************************************************************************
                argument.c  -  Class to encapsulte message arguments
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

Argument *argumentNew()
{
	Argument	*new;

	new = (Argument *) malloc(sizeof(Argument));
	argumentInit(new);

	return new;
}

int argumentInit(Argument *arg)
{
	memset(arg, 0, sizeof(Argument));

	return 0;
}

int argumentSetValue(Argument *arg, DataType type, const void *value)
{
	assert(arg != NULL);

	switch ( type ) {
		case DATATYPE_STRING:
			arg->data.string = malloc(strlen((const char *) value) + 1);
			if ( arg->data.string == NULL )
				return -1;
			strcpy(arg->data.string, (const char *) value);
			printf("argumentSetValue(STRING, %s)\n", value);
			break;
		case DATATYPE_INTEGER:
			arg->data.integer = (int) value;
			printf("argumentSetVAlue(INTEGER, %d)\n", (int) value);
			break;
		case DATATYPE_KEY:
			arg->data.complexData = malloc(sizeof(Key));
			if ( arg->data.complexData == NULL ) {
				printf("DATATYPE_KEY failed\n");
				return -1;
			}
			memcpy(arg->data.complexData, value, sizeof(Key));
			printf("argumentSetValue(KEY, <KEYSTRUCT>)\n");
			break;
		case DATATYPE_KEYSET:
			arg->data.complexData = malloc(sizeof(KeySet));
			if ( arg->data.complexData == NULL )
				return -1;
			memcpy(arg->data.complexData, value, sizeof(KeySet));
			printf("argumentSetValue(KEYSET, <KEYSETSTRUCT>)\n");
			break;
		default:
			printf("UNKNOW TYPE %d\n", type);
			return -1;
	}

	arg->type = type;

	return 0;
}

int argumentClose(Argument *arg)
{
	assert(arg != NULL);

	printf("argumentClose\n");
	
	switch(arg->type) {
		case DATATYPE_STRING:
		case DATATYPE_KEY:
		case DATATYPE_KEYSET:
			printf("free complex\n");
			free(arg->data.complexData);
			printf("freed complex\n");
			break;
	}
	memset(arg, 0, sizeof(Argument));
}

int argumentDel(Argument *arg)
{
	assert(arg != NULL);
	
	printf("argumentDel\n");
	
	argumentClose(arg);
	printf("free arg\n");
	free(arg);
	printf("freed arg\n");
	
}
