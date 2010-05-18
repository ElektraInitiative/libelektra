/***************************************************************************
                protocol.h  -  Interface for a protocol
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
 * Class that implements a protocol.                                       *
 *                                                                         *
 ***************************************************************************/

/* Subversion stuff

$Id$

*/

#ifndef PROTOCOL_H
#define PROTOCOL_H

#include "message.h"

typedef struct {
	unsigned long  magic;
	unsigned int   version;

	size_t    dataLen;
} ProtocolHeader;

int protocolSendMessage(int fd, const Message *msg);
Message *protocolReadMessage(int fd);

#endif /* PROTOCOL_H */
