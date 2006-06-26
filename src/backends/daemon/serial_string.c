/***************************************************************************
                serial_bin.c  -  Low level objects serialization etc
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

$Id: serial_bin.c 788 2006-05-29 16:30:00Z aviram $

*/



#include <stdlib.h>
#include <string.h>

ssize_t serialString_getSize(const void *pChar)
{
	return strblen(pChar);
}


ssize_t serialString_serialize(const void *pChar, void *pBuffer)
{
	ssize_t	len;

	len = serialString_getSize(pChar);
	if ( len != -1 )
		memcpy(pBuffer, pChar, len);

	return len;
}

ssize_t serialString_unserialize(const void *pBuffer, void *ppChar) 
{
	char	**dest;
	ssize_t	len;

	len = strblen(ppChar);
	if ( len != -1 ) {
		*dest = (char *) malloc(len);
		memcpy(*dest, pBuffer, len);
	}

	return len;
}

