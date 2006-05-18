/***************************************************************************
                message.h  -  Interface for a messages
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

#ifndef MESSAGE_H
#define MESSAGE_H

#include "argument.h"



/* Message type */
typedef enum {
	MESSAGE_REQUEST,
	MESSAGE_REPLY
} MessageType;




/* Struct Message */
typedef struct {
	MessageType       type;
	
	unsigned int      procId; /* Procedure ID */
	
	int               nbArgs; /* # args */
	Argument          **args; /* Argument / return parameter */
} Message;




const Argument *messageStealArgByIndex(const Message *msg, int index);
Message *messageNew();
Message *messageNewRequest(int procedure, ...);
int messageInit(Message *msg);
int messageGetProcedureId(const Message *msg);
int messageAddArgument(Message *msg, Argument *argument);
const Argument *messageStealArgByIndex(const Message *msg, int index);
int messageClose(Message *msg);
int messageDel(Message *msg);

#endif /* MESSAGE_H */
