/***************************************************************************
                serial_string.c  -  Low level objects serialization etc
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif
#include <iconv.h>

#include "kdbbackend.h"

/* Daemon works in UTF-8. So, conversion is done
 * only when we're called from a non-UTF8 client */

ssize_t serialString_getSize(const void *pChar)
{
	size_t size;

	size = elektraStrLen(pChar);
	if ( kdbbNeedsUTF8Conversion() ) 
		size = size * 4;

	return size;	
}


ssize_t serialString_serialize(const void *pChar, void *pBuffer)
{
	char	*currentCharset, *writeCursor;
	const char *readCursor;
	iconv_t	converter;
	size_t	bufferSize;
	size_t	size;
	ssize_t	len;

	if ( kdbbNeedsUTF8Conversion() ) {
		currentCharset=nl_langinfo(CODESET);
		converter = iconv_open("UTF-8",currentCharset);

		size = elektraStrLen(pChar);
		bufferSize = size * 4;

		readCursor = (const char *) pChar;
		writeCursor = (char *) pBuffer;
		
		if ( iconv(converter, (ICONV_CONST char **) &readCursor, &size, &writeCursor, &bufferSize) == (size_t)(-1) ) {
			iconv_close(converter);
			return -1;
		}
		iconv_close(converter);
		
		len = writeCursor - ((char *) pBuffer);
	} else {
		len = serialString_getSize(pChar);
		if ( len != -1 ) {
			memcpy(pBuffer, pChar, len);
		}
	}

	return len;
}

ssize_t serialString_unserialize(const void *pBuffer, void *ppChar) 
{
	char    *currentCharset, *writeCursor;
	const char	*readCursor;
	iconv_t converter;
	char	**dest;
	size_t	bufferSize;
	size_t	size;
	ssize_t	len;

	dest = (char **) ppChar;
	if ( kdbbNeedsUTF8Conversion() ) {
		currentCharset=nl_langinfo(CODESET);
		converter = iconv_open(currentCharset, "UTF-8");
		
		size = elektraStrLen(pBuffer);
		bufferSize = size * 4;
		*dest = (char *) malloc(bufferSize);
		readCursor = (const char *) pBuffer;
		writeCursor = (char *) *dest;
		
		if ( iconv(converter, (ICONV_CONST char **) &readCursor, &size, &writeCursor, &bufferSize) == (size_t)(-1) ) {
			iconv_close(converter);
			return -1;
		}
		iconv_close(converter);

		len = writeCursor - ((char *) *dest);	
	} else {
		len = elektraStrLen(pBuffer);
		if ( len != -1 ) {
			*dest = (char *) malloc(len);
			memcpy(*dest, pBuffer, len);
		}
	}

	return len;
}

