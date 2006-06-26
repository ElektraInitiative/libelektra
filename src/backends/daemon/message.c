/***************************************************************************
                message.c  -  Class for a protocol messages
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

/***************************************************************************
 *                                                                         *
 * Class for messages, to be passed over a protocol line.                  *
 *                                                                         *
 ***************************************************************************/



/* Subversion stuff

$Id: message.c 788 2006-05-29 16:30:00Z aviram $

*/


#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "datatype.h"
#include "serial.h"
#include "message.h"

#include "kdb.h"
#include "kdbprivate.h"


Message *messageNew(MessageType msgType, int procedure, ...)
{
	DataType	type;
	va_list		va;
	Message		*msg;
	int		nbArgs;
	char		*buf;
	size_t		size;
	ssize_t		serializedSize;
	
	/* Compute size of Message + serialized args */
	size = sizeof(Message);
	
	va_start(va, procedure);
	type = va_arg(va, DataType);
	nbArgs = 0;
	while ( (type != DATATYPE_LAST) && (nbArgs < MSG_MAX_ARGS) ) {
		serializedSize = serializeGetSize(type, va_arg(va, void *));
		if ( serializedSize == -1 ) {
			va_end(va);
			return NULL;
		}
		fprintf(stderr, "\t\nSize for arg %d tpye %d: %ld\n", nbArgs, type, serializedSize);
		size += serializedSize;
		nbArgs++;
		
		type = va_arg(va, DataType);
	}
	va_end(va);
	
	if ( nbArgs == MSG_MAX_ARGS ) {
		fprintf(stderr, "messageNew(): Too much args\n");
		return NULL;
	}

	/* Allocate some memory */
	msg = (Message *) malloc(size);
	if ( msg == NULL ) {
		perror("messageNew");
		return NULL;
	}
		
	/* Fill message struct */
	memset(msg, 0, size);
	msg->type = msgType;
	msg->procId = procedure;
	msg->nbArgs = nbArgs;
	msg->size = size;
	memset(msg->args, 0, sizeof(msg->args));
	buf = (char *) msg;
	buf += sizeof(Message);
	
	/* Add serialized args ... */
	nbArgs = 0;
	va_start(va, procedure);
	type = va_arg(va, DataType);
	while ( type != DATATYPE_LAST ) {
		serializedSize = serialize(type, va_arg(va, void *), buf);
		if ( serializedSize == -1 ) {
			free(msg);
			va_end(va);
			return NULL;
		}
		msg->args[nbArgs++] = type;
		buf += serializedSize;

		type = va_arg(va, DataType);
	}
	va_end(va);

	return msg;
}

MessageType messageGetType(const Message *msg)
{
	assert(msg != NULL);
	
	return msg->type;
}

int messageGetProcedure(const Message *msg)
{
	assert(msg != NULL);

	return msg->procId;
}

int messageGetNbArgs(const Message *msg)
{
	assert(msg != NULL);

	return msg->nbArgs;
}

/*
 * Extract args from a message
 */
int messageExtractArgs(const Message *msg, ...)
{
	DataType type;
	va_list va;
	const char *buf;
	ssize_t	serializedSize;
	int	args;

	assert(msg != NULL);
	
	/* Skip message struct */
	buf = (const char *) msg;
	buf += sizeof(Message);
		
	/* Unserialize args */
	args = 0;
	va_start(va, msg);
	type = va_arg(va, DataType);
	while ( (type != DATATYPE_LAST) && (args < MSG_MAX_ARGS) ) {
		if ( msg->args[args] != type ) {
			fprintf(stderr, "messageExtract: Type mismatch for args %d. Got %d, expecting %d\n", args, msg->args[args], type);
			va_end(va);
			return -1;
		}
		
		serializedSize = unserialize(type, buf, va_arg(va, void *));
    		if ( serializedSize == -1 ) {
			va_end(va);
			return -1;
		}
		
		buf += serializedSize;
		args++;

		type = va_arg(va, DataType);
	}
	va_end(va);

	return 0;
}

void messageDel(Message *msg)
{
	free(msg);
}
