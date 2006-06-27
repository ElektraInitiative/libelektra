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

#ifndef __MESSAGE__
#define __MESSAGE__

#include "datatype.h"

#define MSG_MAX_ARGS	8

typedef enum {
	MESSAGE_REQUEST,
	MESSAGE_REPLY
} MessageType;

#define INTERNAL_ERROR  1<<15

typedef struct {
	MessageType	type;
	int		procId;
	int		nbArgs;
	DataType	args[MSG_MAX_ARGS];

	size_t		size;

	/* In memory, this struct is followed by
	 * the serialized arguments. "size" reflect that. */
} Message;

Message *messageNew(MessageType msgType, int procedure, ...);
MessageType messageGetType(const Message *msg);
int messageGetProcedure(const Message *msg);
int messageGetNbArgs(const Message *msg);
int messageExtractArgs(const Message *msg, ...);
void messageDel(Message *msg);

#endif /* __MESSAGE__ */
