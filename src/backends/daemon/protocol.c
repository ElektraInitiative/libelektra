/***************************************************************************
                protocol.c  -  Class for a protocol
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

$Id: protocol.c 788 2006-05-29 16:30:00Z aviram $

*/


#include <assert.h>

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "datatype.h"
#include "message.h"

#include "protocol.h"

/* Magic: Magic number for protocol header. Elektra written like l33t ;-) */
#define PROTO_MAGIC     0x0E1E374AL
#define PROTO_VERSION   1

static int     protocolCheckHeader         (const ProtocolHeader *header);


Message *protocolReadMessage(int fd)
{
	Message		*msg;
	ProtocolHeader	header;
	size_t          ret;
	
	/* read header */
	memset(&header, 0, sizeof(header));
	if ( (ret = read(fd, &header, sizeof(header))) == -1 ) {
		perror("protocolReadMessage");
		return NULL;
	}
	if ( protocolCheckHeader(&header) ) {
		fprintf(stderr, "protocolReadMessage(): Incorrect header\n");
		return NULL;
	}

	/* read message */
	msg = (Message *) malloc(header.dataLen);
	if ( msg == NULL ) {
		perror("protocolReadMessage");
		return NULL;
	}
	fprintf(stderr, "protocolReadMessage: %ld read\n", header.dataLen);
	if ( (ret = read(fd, msg, header.dataLen)) == -1 ) {
		perror("protocolReadMessage");
		return NULL;
	}
	
	return msg;
}

int protocolSendMessage(int fd, const Message *message)
{
	ProtocolHeader header;
	ssize_t ret;

	assert(message != NULL);
	
	/* Send header */
	memset(&header, 0, sizeof(header));
	header.magic    = PROTO_MAGIC;
	header.version  = PROTO_VERSION;
	header.dataLen  = message->size;
	if ( (ret = write(fd, &header, sizeof(header))) == -1 ) {
		perror("protocolSendMessage");
		return -1;
	}
	
	/* Send message */
	if ( (ret = write(fd, message, message->size)) == -1 ) {
		perror("protocolSendMessage");
		return -1;
	}
	
	return 0;
}


static int protocolCheckHeader(const ProtocolHeader *header)
{
	assert(header != NULL);

	if ( header->magic != PROTO_MAGIC ) {
		fprintf(stderr, "potocolCheckHeader: Header should be %lx and its %lx\n", PROTO_MAGIC, header->magic);
		return -1;
	}

	if ( header->version < PROTO_VERSION ) {
		fprintf(stderr, "protocolCheckHeader: Protocol version should be %d and its %d\n", PROTO_VERSION, header->version);
		return -1;
	}	

	return 0;
}
