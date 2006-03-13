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

$Id$

*/


#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>

#include "datatype.h"
#include "argument.h"
#include "message.h"

#include "protocol.h"

/* Magic: Magic number for protocol header. Elektra written like l33t ;-) */
#define	PROTO_MAGIC	0x0E1E374A
#define	PROTO_VERSION	1

static ssize_t	protocolSendData        	(int fd, void *data, size_t dataLen);
static ssize_t	protocolReadData        	(int fd, void *data, const ProtocolHeader *header);
static ssize_t	protocolReadDataHeader  	(int fd, ProtocolHeader *header);
static size_t	protocolHeaderGetDataSize	(const ProtocolHeader *header);
static int	protocolCheckHeader		(const ProtocolHeader *header);


ssize_t protocolReadMessage(int fd, Message *msg)
{
	void            *data;
	size_t          ret;
	ProtocolHeader  header;
		
	assert(msg != NULL);
		
	/* read header */
	if ( protocolReadDataHeader(fd, &header) == -1 )
		return -1;
	ret = protocolHeaderGetDataSize(&header);
	if ( (data = malloc(ret)) == NULL )
		return -1;
	
	/* read message */
	if ( (ret = protocolReadData(fd, data, &header)) == -1 ) {
		free(data);
		return -1;
	}
	
	/* Unserialize */
	if ( messageUnserialize(data, msg) == -1 ) {
		free(data);
		return -1;
	}
	
	free(data);
	
	return ret;
}

ssize_t protocolSendMessage(int fd, const Message *msg)
{
	ssize_t ret;
	size_t  len;
	void    *data;
		
	assert(msg != NULL);
	
	/* Message serialization */
	len = messageSerializeGetSize(msg);
	if ( (data = malloc(len)) == NULL )
		return -1;
	
	if ( (ret = messageSerialize(msg, data, len)) == -1 ) {
		free(data);
		return -1;
	}
	
	/* Send message */
	ret = protocolSendData(fd, data, len);
	free(data);
	
	return ret;
}


static ssize_t protocolSendData(int fd, void *data, size_t dataLen)
{
	ProtocolHeader	header;
	ssize_t		ret, read;
	
	assert(data != NULL);

	header.magic	= PROTO_MAGIC;
	header.version	= PROTO_VERSION;
	header.dataLen	= dataLen;

	read = 0;
	
	if ( (ret = write(fd, &header, sizeof(header))) == -1 )
		return -1;
	read += ret;
	
	
	if ( (ret = write(fd, data, dataLen)) == -1 )
		return -1;
	read += ret;

	return ret;
}

static ssize_t protocolReadData(int fd, void *data, const ProtocolHeader *header)
{
	ssize_t	ret;
	
	assert(data != NULL);
	assert(header != NULL);

	return  read(fd, data, header->dataLen);
}

static ssize_t protocolReadDataHeader(int fd, ProtocolHeader *header)
{
	ssize_t	ret;
	
	assert(header != NULL);

	if ( (ret = read(fd, header, sizeof(ProtocolHeader))) == -1 )
		return -1;

	if ( protocolCheckHeader(header) == -1 )
		return -1;

	return ret;
}

static size_t protocolHeaderGetDataSize(const ProtocolHeader *header)
{
	assert(header != NULL);
	
	return header->dataLen;
}

static int protocolCheckHeader(const ProtocolHeader *header)
{
	assert(header != NULL);

	if ( header->magic != PROTO_MAGIC )
		return -1;

	if ( header->version < PROTO_VERSION )
		return -1;

	return 0;
}
