/***************************************************************************
            helper.c  -  Helpers for resolver
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

#include "resolver.h"

#include <kdbextension.h>
#include <kdbprivate.h>

#ifndef ESCAPE_CHAR
#define ESCAPE_CHAR '\\'
#endif

#ifndef KDB_KEY_USERS
/**Users information.
 *
 * This key directory tells you the users existing on the system. */
#define KDB_KEY_USERS            "system/users"
#endif

#define KDB_KEY_USERS_LEN        (sizeof (KDB_KEY_USERS))


/**
 * Locks file for exclusive write mode.
 *
 * This function will block until all reader
 * and writer have left the file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @err sets KDB_ERR_NOLOCK when locking failed
 */
int elektraWriteLock (int fd)
{
	struct flock l;
	int ret=0;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
}

/**
 * Locks file for read mode.
 *
 * Other processes and threads are allowed to read the
 * file too simultaneous.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @err sets KDB_ERR_NOLOCK when locking failed
 */
int elektraReadLock (int fd)
{
	int ret=0;
	struct flock l;
	l.l_type = F_RDLCK; /*Do read Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
}


/**
 * Unlocks file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
int elektraUnlock (int fd)
{
	int ret=0;
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
}

/**
 * Encodes a buffer of data onto hexadecimal ASCII.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * elektraDecode().
 *
 * The @c returned must allocated prior you call this function and won't
 * be bigger than 3 times the size of the source @c elektraDecoded + 1 byte.
 *
 *
 * @param elektraDecoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the preallocated destination for the ASCII-elektraEncoded data.
 * @return the amount of bytes used in the resulting elektraEncoded buffer.
 * @see elektraDecode()
 * @ingroup backendhelper
 */
ssize_t elektraEncode(void *elektraDecoded, size_t size, char *returned)
{
	char *readCursor=elektraDecoded;
	char *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;
	ssize_t ssize = size;

	if (size > SSIZE_MAX) return -1;

	if (ssize == 0 )
		return 0;
	
	while ((readCursor-(char *)elektraDecoded)<ssize)
	{
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
 * UnelektraEncodes a buffer of ASCII hexadecimal values into a byte stream.
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * 
 * The @c returned data won't be bigger than half the size of the
 * source @c elektraEncoded data.
 *
 * @param elektraEncoded the source of ASCII hexadecimal digits.
 * @param returned preallocated destination for the elektraDecoded data.
 * @return the amount of bytes elektraDecoded
 * @return -1 on failure
 * @see elektraEncode()
 * @ingroup backendhelper
 */
ssize_t elektraDecode(char *elektraEncoded,void *returned)
{
	char byteInHexa[5]="0x";
	char *readCursor=elektraEncoded;
	char *writeCursor=returned;

	if (!elektraEncoded) {
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
			/*errno=KDB_ERR_TYPEMISMATCH;*/
			return -1;
		}
	}
	return (long int)writeCursor-(long int)returned;
}


/**
 * Char encoding.
 *
 * Encode '/', '\', '%', '+', ' ' char following
 * RFC 2396 or copy char untouched if different.
 *
 * @param c Char to elektraEncode
 * @param buffer string wich will contain elektraEncoded char
 * @param bufSize Size of the buffer
 * @return: Size of the elektraEncoded string if success or -1
 * if error  * (then buffer is untouched)
 * @ingroup backendhelper
 * @see elektraDecodeChar
 *
 * NOTE: No '\\0' is added at the end of buffer.
 *
 */
int elektraEncodeChar(char c, char *buffer, size_t bufSize)
{
	switch(c) {
		case '%':
			if ( bufSize >= (3*sizeof(char)) ) {
				memcpy(buffer, "%25", sizeof("%25"));
				return (3*sizeof(char));
			}
			return -1;

		case '+':
			if ( bufSize >= (3*sizeof(char)) ) {
				memcpy(buffer, "%2B", sizeof("%2B"));
				return (3*sizeof(char));
			}
			return -1;

		case ' ':
			if ( bufSize >= 1*sizeof(char) ) {
				*(buffer) = '+';
				return (1*sizeof(char));
			}
			return -1;

		case '/':
			if ( bufSize >= (3*sizeof(char)) ) {
				memcpy(buffer, "%2F", sizeof("%2F"));
				return (3*sizeof(char));
			}
			return -1;

		case '\\':
			if ( bufSize >= (3*sizeof(char)) ) {
				memcpy(buffer, "%5C", sizeof("%5C"));
				return (3*sizeof(char));
			}
			return -1;

		default:
			if ( bufSize >= (1*sizeof(char)) ) {
				*(buffer++) = c;
				return (1*sizeof(char));
			}
			return -1;
	}

	return 0;
}

/**
 * Char decoding.
 *
 * Decode one char from %25, %2B, %2F, %2C following
 * RFC 2396 or copy char untouched if different.
 *
 * @param from String containing sequence to decode
 * @param into Decoded char
 * @return: Positive size of byte read from "from" for decoding
 * the sequence if sucess or -1 if error (into untouched)
 * @ingroup backendhelper
 * @see elektraEncodeChar
 *
 * NOTE: No '\\0' is added at the end of buffer.
 *
 */
int elektraDecodeChar(const char *from, char *into)
{
	switch(*from) {
		case '%':
			if ( strlen(from) >= (3*sizeof(char)) ) {
				switch(*(from+2)) {
					case '5':       *into = '%';    break;
					case 'B':       *into = '+';    break;
					case 'F':       *into = '/';    break;
					case 'C':       *into = '\\';   break;

					default:
						return -1;
				}

				return (3*sizeof(char));
			}
			return -1;

		case '+':
			*into = ' ';
			return (1*sizeof(char));

		default:
			*into = *from;
			return (1*sizeof(char));
	}

	return 0;
}

/**
 * Translate a relative file name to a key name
 * applying decoding.
 *
 * @param string Filename
 * @param buffer decoded keyName
 * @param bufSize Size of buffer
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @see elektraKeyNameToRelativeFilename
 *
 */
int elektraFilenameToKeyName(const char *string, char *buffer, size_t bufSize)
{
	char decoded;
	int j;

	while (*string != '\0')
	{
		if (bufSize <= sizeof(char))
		{
			/*errno = KDB_ERR_TOOLONG;*/
			return -1;
		}

		if ( *string == PATH_SEPARATOR ) {
			/* Translate PATH_SEPARATOR into KEY_SEPARATOR */
			*(buffer++) = KEY_SEPARATOR;
			bufSize -= sizeof(char);
			string++;
		} else {
			/* Decode char */
			if ( (j = elektraDecodeChar(string, &decoded)) != -1 ) {
				string += j;
				*(buffer++) = decoded;
				bufSize -= sizeof(char);
			} else {
				*(buffer) = '\0';
				/*errno = KDB_ERR_CONVERT;*/
				return -1;
			}
		}
	}

	*buffer = '\0';

	return 0;
}

/**
 * Translate a key name to a relative file name
 * applying encoding.
 *
 * @param string Keyname
 * @param buffer elektraEncoded filename
 * @param bufSize Size of buffer
 * @return Number of byte written in buffer on success,
 * @return -1 on failure
 * @ingroup backendhelper
 * @see elektraKeyNameToRelativeFilename
 *
 **/
int elektraKeyNameToRelativeFilename(const char *string, char *buffer, size_t bufSize)
{
	size_t	written;
	int     j;

	written = 0;
	while (*string != '\0')
	{
		if (bufSize <= sizeof(char))
		{
			/*errno = KDB_ERR_TOOLONG;*/
			return -1;
		}

		if ( *string == ESCAPE_CHAR && *(string+1) == PATH_SEPARATOR ) {
			/* Key delimiter escaped, elektraEncode these two (escape + delim) */
			if ( (j = elektraEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				buffer += j;
				written += j*sizeof(char);
			} else {
				/*errno = KDB_ERR_CONVERT;*/
				return -1;
			}

			if ( (j = elektraEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				written += j*sizeof(char);
				buffer += j;
			} else {
				/*errno = KDB_ERR_CONVERT;*/
				return -1;
			}

		} else if ( *string == PATH_SEPARATOR ) {
			/* Replace unescaped KEY_DELIM to PATH_SEPARATOR */
			*(buffer++) = PATH_SEPARATOR;
			bufSize -= sizeof(char);
			written += sizeof(char);
			string++;

		} else {
			/* Encode ... */
			if ( (j = elektraEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				written += j*sizeof(char);
				buffer += j;
			} else {
				/*errno = KDB_ERR_CONVERT;*/
				return -1;
			}
		}
	}
	*buffer = '\0';
	written++;

	return written;
}

/**
 * Calculate the real file name for a key.
 *
 * system/ keys will get the prefix KDB_DB_SYSTEM
 *
 * For the user/ keys the algorithm works as follow:
 * 1.) When the override environment KDB_HOME exists the configuration will be searched below KDB_HOME/KDB_DB_USER
 * 2.) When the owner of the key exists in the elektra user database steps a.) and b.) will be tested:
 *    a.) The specific value for configuration storage of the user below system/users/\<owner\>/kdb
 *    b.) The home variable in system/users/\<owner\>/home will be merged together with KDB_DB_USER
 * 3.) When the environment HOME exists the configuration will be searched below HOME/KDB_DB_USER
 * 4.) Otherwise the KDB_DB_HOME/\<owner\>/KDB_DB_USER will be used
 *
 * @param forKey the key object to work with
 * @param handle the kdb handle to work with
 * @param returned the buffer to return the calculated filename
 * @param maxSize maximum number of bytes that fit the buffer
 * @see kdbCalcRelativeFilename()
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup backendhelper
 * @return length of returned string on success
 * @return -1 on failure
 */
ssize_t elektraGetFullFilename(const Key *forKey, char *returned, size_t maxSize)
{
	size_t length=0;
	char * home;
	char buffer [MAX_PATH_LENGTH] = KDB_KEY_USERS;
	ssize_t rc;

	switch (keyGetNamespace(forKey)) {
		case KEY_NS_SYSTEM: {
			/* Prepare to use the 'system/ *' database */
			strncpy(returned,KDB_DB_SYSTEM,maxSize);
			length=strlen(returned);
			break;
		}
		/* If we lack a usable concept of users we simply let the default handle it
		 * and hence disable the entire user/ hiarchy. */
		case KEY_NS_USER: {

			if ((home = getenv("KDB_DIR")) != 0)
			{
				length=snprintf(returned,maxSize,"%s",home);
				break;
			}

			if ((home = getenv("KDB_HOME")) != 0)
			{
				length=snprintf(returned,maxSize,"%s/%s",home,KDB_DB_USER);
				break;
			}

			strcat (buffer, "/");
			strcat (buffer, keyOwner(forKey));
			strcat (buffer, "/kdb");
			length=0; /*kdbGetString (handle, buffer, returned, maxSize)-1;*/
			if (length > 0)
			{
				break;
			}

			strcpy (buffer, KDB_KEY_USERS);
			strcat (buffer, "/");
			strcat (buffer, keyOwner(forKey));
			strcat (buffer, "/home");
			length=0; /*kdbGetString (handle, buffer, returned, maxSize)-1;*/
			if (length > 0)
			{
				strcpy (buffer, returned);
				length=snprintf(returned,maxSize,"%s/%s",buffer, KDB_DB_USER);
				break;
			}

			if ((home = getenv("HOME")) != 0)
			{
				length=snprintf(returned,maxSize,"%s/%s",home,KDB_DB_USER);
				break;
			}

			length=snprintf(returned,maxSize,"%s/%s/%s",KDB_DB_HOME, keyOwner(forKey), KDB_DB_USER);
			break;
		}

		default: {
			/*errno=KDB_ERR_INVALIDKEY;*/
			return -1;
		}
	}

	returned[length]='/'; length++;
	if (maxSize <= length)
	{
		/*errno=KDB_ERR_TOOLONG;*/
		return -1;
	}
	// TODO !! rc=elektraKeyCalcRelativeFilename(forKey,returned+length,maxSize-length);
	rc = elektraKeyNameToRelativeFilename(keyName(forKey), returned+length, maxSize-length);

	if (rc == -1) return -1;
	else length += rc;

	return length;
}

