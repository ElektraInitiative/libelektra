/***************************************************************************
            helpers.c  -  Helpers for backends
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/* Subversion stuff

$Id: libkdb.c 736 2006-04-14 15:31:44Z aviram $

*/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ICONV
#include <iconv.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif



#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <pthread.h>


/* kdbbackend.h will include kdb.h and kdbprivate.h */
#include "kdbbackend.h"
#include "kdbLibLoader.h"

/* usleep doesn't exist on win32, so we use Sleep() */
#ifdef WIN32
#define usleep(x) Sleep(x)
#endif

#ifdef HAVE_FCNTL_H
/**
 * Locks file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure and errno is set (fcntl)
 * @see encode()
 * @ingroup backend
 */
int lock (int fd)
{
	struct flock l;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	return fcntl (fd, F_SETLKW, &l);
}


/**
 * Unlocks file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure and errno is set (fcntl)
 * @see encode()
 * @ingroup backend
 */
int unlock (int fd)
{
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	return fcntl (fd, F_SETLKW, &l);
}
#endif



/**
 * Unencodes a buffer of ASCII hexadecimal values into a byte stream.
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * 
 * The @c returned data won't be bigger than half the size of the
 * source @c encoded data.
 *
 * @param encoded the source of ASCII hexadecimal digits.
 * @param returned preallocated destination for the unencoded data.
 * @return the amount of bytes unencoded
 * @return -1 on failure and @c errno
 * 	is set to KDB_RET_TYPEMISMATCH
 * @see encode()
 * @ingroup backend
 */
ssize_t unencode(char *encoded,void *returned) {
	char byteInHexa[5]="0x";
	char *readCursor=encoded;
	char *writeCursor=returned;

	if (!encoded) {
		if (returned) *(char *)returned=0;
		return 0;
	}

	byteInHexa[4]=0;
	while (*readCursor) {
		if (isspace((int)*readCursor)) 
		{
		readCursor++;
		continue;
		}
		if (isxdigit((int)*readCursor)) {
			long int converted;
			byteInHexa[2]=readCursor[0];
			byteInHexa[3]=readCursor[1];
			converted=strtol(byteInHexa,0,16); /* convert from hexa to a byte */
			*writeCursor=(unsigned char)converted;

			readCursor+=2;
			writeCursor++;
		} else {
			/* This is suposed to be a hex-digit stream. But is not, so return. */
			errno=KDB_RET_TYPEMISMATCH;
			return -1;
		}
	}
	return (long int)writeCursor-(long int)returned;
}

/**
 * Checks if UTF-8 conversion is needed in current context.
 * if nl_langinfo() is not available, no conversion is ever needed.
 * If iconv usage is disabled there is no need to check if we need to convert.
 * Furthermore, some systems have nl_langinfo(), but lacks ability to get
 * CODESET through it.
 * Look at the comments by the UTF8Engine() function for more information.
 *
 * @return 0 if not needed
 * @return anything else if needed
 * @ingroup backend
 */
int kdbNeedsUTF8Conversion() {
#if defined(HAVE_NL_LANGINFO) && defined(HAVE_ICONV) && defined(CODESET)
	return strcmp(nl_langinfo(CODESET),"UTF-8");
#else
	return 0;
#endif
}


/**
 * Converts string to (@p direction = @c UTF8_TO) and from
 * (@p direction = @c UTF8_FROM) UTF-8.
 * 
 * Since Elektra provides portability for key names and string values between
 * different codesets, you should use this helper in your backend to convert
 * to and from universal UTF-8 strings, when storing key names, values and
 * comments.
 *
 * If iconv() or nl_langinfo() is not available on your system, or if iconv()
 * usage is disabled (--disable-iconv on build time) simply return 0
 * immediately.
 *
 * @param direction must be @c UTF8_TO (convert from current non-UTF-8 to
 * 	UTF-8) or @c UTF8_FROM (convert from UTF-8 to current non-UTF-8)
 * @param string before the call: the string to be converted; after the call:
 * 	reallocated to carry the converted string
 * @param inputOutputByteSize before the call: the size of the string including
 * 	leading NULL; after the call: the size of the converted string including
 * 	leading NULL
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup backend
 *
 */
int UTF8Engine(int direction, char **string, size_t *inputOutputByteSize) {
/* Current solution is not very complete.
 * Iconv might well be available when a usable nl_langinfo is not.
 * In this case we it should be possible to determine charset through other means
 * See http://www.cl.cam.ac.uk/~mgk25/unicode.html#activate for more info on a possible solution */
 
#if defined(HAVE_ICONV) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
	char *currentCharset=0;
	char *converted=0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;
	
	if (kdbNeedsUTF8Conversion() && *inputOutputByteSize) currentCharset=nl_langinfo(CODESET);
	else return 0;

	if (direction==UTF8_TO) converter=iconv_open("UTF-8",currentCharset);
	else converter=iconv_open(currentCharset,"UTF-8");

	if (converter == (iconv_t)(-1)) return -1;

	/* work with worst case, when all chars are wide */
	bufferSize=*inputOutputByteSize * 4;
	converted=malloc(bufferSize);
	if (!converted) return -1;

	readCursor=*string;
	writeCursor=converted;
	/* On some systems and with libiconv, arg1 is const char **. 
	 * ICONV_CONST is defined by configure if the system needs this */
	if (iconv(converter,
			(ICONV_CONST char **)&readCursor,inputOutputByteSize,
			&writeCursor,&bufferSize) == (size_t)(-1)) {
		free(converted);
		iconv_close(converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputOutputByteSize=writeCursor-converted;
	/* store the current unencoded string for future free */
	readCursor=*string;
	/* allocate an optimal size area to store the converted string */
	*string=malloc(*inputOutputByteSize);
	/* copy all that matters for returning */
	memcpy(*string,converted,*inputOutputByteSize);
	/* release memory used by passed string */
	free(readCursor);
	/* release buffer memory */
	free(converted);
	/* release the conversor engine */
	iconv_close(converter);
#endif
	return 0;
}



/**
 * Encodes a buffer of data onto hexadecimal ASCII.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * unencode().
 *
 * The @c returned must allocated prior you call this function and won't
 * be bigger than 3 times the size of the source @c unencoded + 1 byte.
 *
 *
 * @param unencoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the preallocated destination for the ASCII-encoded data.
 * @return the amount of bytes used in the resulting encoded buffer.
 * @see unencode()
 * @ingroup backend
 */
ssize_t encode(void *unencoded, size_t size, char *returned) {
	char *readCursor=unencoded;
	char *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;

	if ( size == 0 )
		return 0;
	
	while ((readCursor-(char *)unencoded)<size) {
		sprintf(writeCursor,"%02x",*(unsigned char *)readCursor);
		readCursor++;
		writeCursor+=2;
		currentInBlock++;
		currentInLine++;
		if (currentInLine==lineStep) {
			*writeCursor='\n'; writeCursor++;
			currentInLine=0;
			currentInBlock=0;
		}
		if (currentInBlock==blockStep) {
			*writeCursor=' '; writeCursor++;
			currentInBlock=0;
		}
	}
	*writeCursor='\n';
	*++writeCursor=0;
	return writeCursor-returned;
}


/**
 * An inefficient implementation for the kdbRename() method.
 * If backend doesn't want to reimplement this method, this
 * implementation can be used, in which kdbSetKey()/kdbRemoveKey()
 * will be called for the key.
 *
 * @see kdbRename() for expected behavior.
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup backend
 */
int kdbRename_default(KDBHandle handle, Key *key, const char *newName)
{
	Key	*newKey;

	if ( (newKey = keyNew(KEY_SWITCH_END)) == NULL )
		return -1;
	
	if ( keyDup(key, newKey) ) {
		keyDel(newKey);
		return -1;
	}
	
	if ( keySetName(newKey, newName) ) {
		/* Create the new key */
		if ( kdbSetKey(handle, newKey) ) {
			keyDel(newKey);
			return -1;
		}
		keyDel(newKey);
		
		/* Remove the old one ... */
		if ( kdbRemoveKey(handle, key) ) 
			return -1;

		return 0;
	} else {
		/* newName isn't valid or empty */
		keyDel(newKey);
		return -1;	
	}
}

/**
 * A probably inefficient implementation for the kdbSetKeys()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used, in which kdbSetKey() will be called for
 * each Key object contained in @p ks.
 *
 * If some error occurs, kdbSetKeys_default() will stop. In this situation the KeySet
 * internal cursor is left on the key that generated the error.
 *
 * @see kdbSetKeys(), kdbSetKeys_backend()
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 *
 * @ingroup backend
 */
int kdbSetKeys_default(KDBHandle handle, KeySet *ks) {
	Key *current=ksCurrent(ks);
	int ret;

	if (!current) current=ksNext(ks);
	while (current) {
		if (keyNeedsSync(current))
			if ((ret=kdbSetKey(handle,current))) /* check error */
				return ret;
		
		current=ksNext(ks);
	}

	return 0;
}





/**
 * A high level, probably inefficient, implementation for the kdbMonitorKey()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used.
 *
 * @ingroup backend
 */
uint32_t kdbMonitorKey_default(KDBHandle handle, Key *interest,
		uint32_t diffMask, unsigned long iterations, unsigned sleeptime) {
	Key *tested;
	int rc;
	uint32_t diff;
	int infinitum=0;

	/* consistency */
	if (!interest || !keyGetNameSize(interest)) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleeptime) sleeptime=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	/* Work with a copy of the key */
	tested=keyNew(0);
	keyDup(interest,tested);

	while (infinitum || --iterations) {
		rc=kdbGetKey(handle,tested);
		if (rc) {
			/* check what type of problem happened.... */
			switch (errno) {
				case KDB_RET_NOCRED:
					keyDel(tested);
					return KEY_SWITCH_NEEDSYNC;
				case KDB_RET_NOTFOUND:
					keyDel(tested);
					return KEY_SWITCH_FLAG;
			}
		}
		
		diff=keyCompare(tested,interest);
		
		if (diff & diffMask) {
			/* If differences interests us, return it, otherwise cycle again.
			 * We don't loose the original key context in a KeySet because
			 * we worked with a copy of the key.
			 */
			keyDup(tested,interest);
			keyDel(tested);
			return diff;
		}
		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleeptime);
	}
	
	keyDel(tested);

	return 0;
}

