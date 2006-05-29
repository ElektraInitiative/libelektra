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

$Id$

*/



#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "datatype.h"
#include "argument.h"
#include "message.h"




/**
 * Create a new message
 *
 * @return A newly allocated Message
 */
Message *messageNew() {
	Message	*new;

	new = (Message *) malloc(sizeof(Message));
	if ( new == NULL )
		return NULL;
	memset(new, 0, sizeof(Message));

	return new;
}





/**
 * Create a new request
 *
 * Convenience method for create new request simply.
 *
 * @param procedure ProcedureId
 * @param va_arg Set of DataType, VALUE
 *
 * @return new Message
 */
Message *messageNewRequest(int procedure, ...) {
	va_list     va;
	DataType    type;
	Message     *msg;
	Argument    *arg;

	printf("messageNewReques(%d);\n");
	
	/* Build the request */
	msg = messageNew();
	msg->type = MESSAGE_REQUEST;
	msg->procId = procedure;

	va_start(va, procedure);
	
	/* Add arguments */
	type = va_arg(va, DataType);
	while ( type != DATATYPE_LAST ) {
		arg = argumentNew();
		if ( arg == NULL ) {
			va_end(va);
			messageDel(msg);
			return NULL;
		}
		printf("%\t -> ");
		argumentSetValue(arg, type, va_arg(va, const void *));

		if ( messageAddArgument(msg, arg) == -1 ) {
			printf("UNABLE TO ADD ARGUMENT\n");
			va_end(va);
			messageDel(msg);
			return NULL;
		}
		
		type = va_arg(va, DataType);
	}

	va_end(va);
	
	return msg;	
}





int messageInit(Message *msg) {
	assert(msg != NULL);
	
	memset(msg, 0, sizeof(Message));
	
	return 0;
}





/**
 * Get procedure ID of a message
 *
 * @param msg Message
 * 
 * @return Procedure ID
 */
int messageGetProcedureId(const Message *msg) {
	assert(msg != NULL);

	return msg->procId;
}

/**
 * Add an argument to a Message
 *
 * @param msg Message where argument will be added
 * @param argument Argument to add
 *
 * @return 0 on success / -1 on error
 */
int messageAddArgument(Message *msg, Argument *argument) {
	Argument	*new;
	Argument	**newArgs;

	assert(msg != NULL);
	assert(argument != NULL);

	printf("messageAddArgument()\n");
	
	/* Extend array for the new argument */
	newArgs = (Argument **) realloc(msg->args, (msg->nbArgs + 1) * sizeof(Argument *));
	if ( newArgs == NULL ) {
		perror("realloc");
		return -1;
	}
	msg->args = newArgs;

	msg->args[msg->nbArgs] = argument;
	msg->nbArgs++;

	return 0;
}





/**
 * Get the Argument from a message
 *
 * @param msg Message
 * @param index Index of the argument inside the message
 *
 * @return Argument or NULL if not found
 */
const Argument *messageStealArgByIndex(const Message *msg, int index) {
	assert(msg != NULL);

	if ( index < msg->nbArgs )
		return msg->args[index];

	return NULL;
}




/**
 * Finish the usage of a Message
 *
 * Free all arguments 
 */
int messageClose(Message *msg) {
	int	i;

	assert(msg != NULL);
	
	printf("messageClose(%ld)\n", msg->procId);
	
	for(i = 0 ; i<msg->nbArgs ; i++) {
		printf("\t ->");
		argumentDel(msg->args[i]);
	}
	
	free(msg->args);
	memset(msg, 0, sizeof(Message));

	return 0;
}




int messageDel(Message *msg) {
	assert(msg != NULL);
	printf("messageDel(%ld)\n", msg->procId);
	
	messageClose(msg);
	free(msg);

	return 0;
}

