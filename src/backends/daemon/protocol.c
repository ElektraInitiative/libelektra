/***************************************************************************
                protocol.c  -  Class for a protocol
                             -------------------
    begin                : Sun Mar 12 2006
    copyright            : (C) 2006 by Yannick Lecaillez, Avi Alkalay
    email                : sizon5@gmail.com, avi@unix.sh
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "datatype.h"
#include "message.h"

#include "protocol.h"

/* Magic: Magic number for protocol header. Elektra written like l33t ;-) */
#define PROTO_MAGIC     0x0E1E374AL
#define PROTO_VERSION   1

static int     protocolCheckHeader         (const ProtocolHeader *header);


/**
 * @defgroup protocol Protocol
 * @brief Protocol provide methods for transport Message throught a medium.
 * 
 * Protocol allow to send & receive @link message Message@endlink securely. It does some sanity check
 * like checking protocol version & a magic number user for verify @link message message@endlink boundaries.
 *
 */

/**
 * Read a message from file/socket fd 
 * This methods check header, read data and
 * create a new Message object from data.
 *
 * @param fd File Descriptor where data should be readen
 * @return a newly allocated message (which must be freed
 * using messageDel()) or NULL if error occured.
 *
 * @see protocolCheckHeader()
 * @see protocolSendMessage()
 *
 * @ingroup protocol
 */
Message *protocolReadMessage(int fd)
{
	Message		*msg;
	char		*buf;
	ProtocolHeader	header;
	size_t		toRead;
	ssize_t          ret;
	
	/* read header */
	memset(&header, 0, sizeof(header));
	if ( (ret = read(fd, &header, sizeof(header))) == -1 ) {
		perror("protocolReadMessage");
		return NULL;
	}
	
	if ( protocolCheckHeader(&header) ) { 
		perror("protocolCheckHeader");
		return NULL;
	}

	/* read message */
	msg = (Message *) malloc(header.dataLen);
	if ( msg == NULL ) {
		perror("malloc");
		return NULL;
	}
	
	buf = (char *) msg;
	toRead = header.dataLen;
	while ( toRead > 0 ) {
		if ( (ret = read(fd, buf, toRead)) == -1 ) {
			perror("protocolReadMessage");
			return NULL;
		}

		toRead -= ret;
		buf += ret;
	}

	return msg;
}

/**
 * Write a specified message to a file/socket
 * This methods build a header, and write message to the descriptor.
 * 
 * @param fd File Descriptor where data should be written
 * @param message Message to send.
 * @return 0 if OK, -1 otherwise
 *
 * @see protocolReadMessage()
 * @ingroup protocol
 */
int protocolSendMessage(int fd, const Message *message)
{
	ProtocolHeader header;
	const char	*buf;
	size_t	toWrite;
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
	toWrite = message->size;
	buf = (const char *) message;
	while ( toWrite > 0 ) {
		if ( (ret = write(fd, buf, message->size)) == -1 ) {
			perror("protocolSendMessage");
			return -1;
		}

		toWrite -= ret;
		buf += ret;
	}
	
	return 0;
}

/**
 * Check header validity
 * This methods check a magic number for detect data boundaries.
 * Check a version number for compatibility between protocol version.
 *
 * @param header Header to check
 * @return 0 if OK, -1 otherwise
 *
 * @see protocolReadMessage()
 */
static int protocolCheckHeader(const ProtocolHeader *header)
{
	assert(header != NULL);

	if ( header->magic != PROTO_MAGIC ) {
		fprintf(stderr, "protocolCheckHeader: Got %lx expected %lx", header->magic, PROTO_MAGIC);
		errno = EINVAL;
		return -1;
	}

	if ( header->version < PROTO_VERSION ) {
		errno = EINVAL;
		return -1;
	}	

	return 0;
}
