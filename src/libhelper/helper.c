/***************************************************************************
            helper.c  -  Helpers for backends
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


#include "helper.h"

/**
 * @defgroup backendhelper KDB Backends :: Backend Helper for Elektra
 * @brief Backend helper Methods for Elektra and Backends.
 *
 * To use them:
 * @code
 * #include <kdbbackend.h>
 * @endcode
 *
 * These backend helper methods provide
 * functionality commonly used by backends to make backend
 * development easier and to provide the same behaviour
 * between backends.
 *
 */


/**
 * Locks file for exclusive write mode.
 *
 * This function will block until all reader
 * and writer have left the file.
 *
 * @param f is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @err sets KDB_ERR_NOLOCK when locking failed
 */
int kdbbWriteLock (FILE *f)
{
#ifdef HAVE_FCNTL_H
	int fd = fileno(f);
	struct flock l;
	int ret=0;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
#else
	return 0;
#endif
}

/**
 * Locks file for read mode.
 *
 * Other processes and threads are allowed to read the
 * file too simultaneous.
 *
 * @param f is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @err sets KDB_ERR_NOLOCK when locking failed
 */
int kdbbReadLock (FILE *f)
{
#ifdef HAVE_FCNTL_H
	int ret=0;
	int fd = fileno(f);
	struct flock l;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
#else
	return 0;
#endif
}


/**
 * Unlocks file.
 *
 * @param f is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 * @err sets KDB_ERR_NOLOCK when locking failed
 */
int kdbbUnlock (FILE *f)
{
#ifdef HAVE_FCNTL_H
	int ret=0;
	int fd = fileno(f);
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
#else
	return 0;
#endif
}



/**
 * Encodes a buffer of data onto hexadecimal ASCII.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * kdbbDecode().
 *
 * The @c returned must allocated prior you call this function and won't
 * be bigger than 3 times the size of the source @c kdbbDecoded + 1 byte.
 *
 *
 * @param kdbbDecoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the preallocated destination for the ASCII-kdbbEncoded data.
 * @return the amount of bytes used in the resulting kdbbEncoded buffer.
 * @see kdbbDecode()
 * @ingroup backendhelper
 */
ssize_t kdbbEncode(void *kdbbDecoded, size_t size, char *returned)
{
	char *readCursor=kdbbDecoded;
	char *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;
	ssize_t ssize;

	if (size > SSIZE_MAX) return -1;
	ssize = size;

	if ( size == 0 )
		return 0;
	
	while ((readCursor-(char *)kdbbDecoded)<ssize)
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
 * UnkdbbEncodes a buffer of ASCII hexadecimal values into a byte stream.
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * 
 * The @c returned data won't be bigger than half the size of the
 * source @c kdbbEncoded data.
 *
 * @param kdbbEncoded the source of ASCII hexadecimal digits.
 * @param returned preallocated destination for the kdbbDecoded data.
 * @return the amount of bytes kdbbDecoded
 * @return -1 on failure
 * @see kdbbEncode()
 * @ingroup backendhelper
 */
ssize_t kdbbDecode(char *kdbbEncoded,void *returned)
{
	char byteInHexa[5]="0x";
	char *readCursor=kdbbEncoded;
	char *writeCursor=returned;

	if (!kdbbEncoded) {
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
 * Checks if UTF-8 conversion is needed in current context.
 * if nl_langinfo() is not available, no conversion is ever needed.
 * If iconv usage is disabled there is no need to check if we need to convert.
 * Furthermore, some systems have nl_langinfo(), but lacks ability to get
 * CODESET through it.
 * Look at the comments by the kdbbUTF8Engine() function for more information.
 *
 * @return 0 if not needed
 * @return anything else if needed
 * @ingroup backendhelper
 */
int kdbbNeedsUTF8Conversion()
{
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
 * Broken locales in applications can cause problems too. Make sure to load
 * the environment locales in your application using
 * @code
setlocale (LC_ALL, "");
 * @endcode
 *
 * Otherwise kdbbUTF8Engine will quit with -1 leading that backends return
 * with error when non-ascii characters appear. Binary values are not effected.
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
 * @return -1 on failure
 * @ingroup backendhelper
 *
 */
int kdbbUTF8Engine(int direction, char **string, size_t *inputOutputByteSize)
{
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
	
	if (kdbbNeedsUTF8Conversion() && *inputOutputByteSize) currentCharset=nl_langinfo(CODESET);
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
	/* store the current kdbbDecoded string for future free */
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
 * Char encoding.
 *
 * Encode '/', '\', '%', '+', ' ' char following
 * RFC 2396 or copy char untouched if different.
 *
 * @param c Char to kdbbEncode
 * @param buffer string wich will contain kdbbEncoded char
 * @param bufSize Size of the buffer
 * @return: Size of the kdbbEncoded string if success or -1
 * if error  * (then buffer is untouched)
 * @ingroup backendhelper
 * @see kdbiDecodeChar
 *
 * NOTE: No '\\0' is added at the end of buffer.
 *
 */
int kdbbEncodeChar(char c, char *buffer, size_t bufSize)
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
 * @see kdbbEncodeChar
 *
 * NOTE: No '\\0' is added at the end of buffer.
 *
 */
int kdbbDecodeChar(const char *from, char *into)
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
 * @see kdbbKeyNameToRelativeFilename
 *
 */
int kdbbFilenameToKeyName(const char *string, char *buffer, int bufSize)
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
			if ( (j = kdbbDecodeChar(string, &decoded)) != -1 ) {
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

/**Calculates the keyname out of a relative filename.
 *
 * @param handle The kdb handle to work with
 * @param forFilename needs to be the a null terminated string containing the relative filename
 * @param parentKey is the key above the key which will be returned
 * @param returned The proper keyname and owner will be stored in returned. A valid key must be passed.
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup backendhelper
 * @return length of returned string on success
 * @return -1 on failure
 * @see kdbbKeyNameToRelativeFilename()
 */
ssize_t kdbbGetFullKeyName (KDB *handle, const char *forFilename, const Key *parentKey, Key *returned)
{
	size_t size=0;
	char *transformedName=0;
	char *name;

	/* Next 2 ifs are required to transform filename from UTF-8 */
	transformedName = malloc(size=kdbiStrLen(forFilename));
	strcpy(transformedName,forFilename);

	if (kdbbUTF8Engine(UTF8_FROM,&transformedName,&size)) {
		free(transformedName);
		/*errno = KDB_ERR_CONVERT;*/
		return -1; 
	}

	/* Translate from filename -> keyname and concat it into name */
	name = (char *) malloc(size*3 + keyGetNameSize(parentKey));
	strcpy (name, keyName(parentKey));
	name[keyGetNameSize(parentKey)-1] = '/';
	kdbbFilenameToKeyName(transformedName, name+keyGetNameSize(parentKey), size*3);

	/* Now set the name and owner */
	keySetName (returned, name);
	keySetOwner(returned, keyOwner(parentKey));

	free(transformedName);
	free(name);
	return 0;
}

/**
 * Translate a key name to a relative file name
 * applying encoding.
 *
 * @param string Keyname
 * @param buffer kdbbEncoded filename
 * @param bufSize Size of buffer
 * @return Number of byte written in buffer on success,
 * @return -1 on failure
 * @ingroup backendhelper
 * @see kdbbKeyNameToRelativeFilename
 *
 **/
int kdbbKeyNameToRelativeFilename(const char *string, char *buffer, size_t bufSize)
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
			/* Key delimiter escaped, kdbbEncode these two (escape + delim) */
			if ( (j = kdbbEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				buffer += j;
				written += j*sizeof(char);
			} else {
				/*errno = KDB_ERR_CONVERT;*/
				return -1;
			}

			if ( (j = kdbbEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
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
			if ( (j = kdbbEncodeChar(*(string++), buffer, bufSize)) != -1 ) {
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
 * This is a helper to kdbGetFullFilename()
 *
 * @param key has the relevant name for the relative filename
 * @param relativeFilename the buffer to return the calculated filename
 * @param maxSize maximum number of bytes that fit the buffer
 * @see kdbGetFullFilename()
 * @return number of bytes written to the buffer
 * @return -1 on failure
 * @ingroup backendhelper
 */
ssize_t kdbbKeyCalcRelativeFilename(const Key *key,char *relativeFilename,size_t maxSize)
{
	if (kdbbNeedsUTF8Conversion()) {
		char *converted;
		size_t size;

		if (!(size=keyGetNameSize(key))) return -1;

		converted = (char *) malloc(MAX_PATH_LENGTH);
		size = kdbbKeyNameToRelativeFilename(keyName(key), converted,
			MAX_PATH_LENGTH);

/* 		memcpy(converted,relativeFilename,convertedSize); */

		if (kdbbUTF8Engine(UTF8_TO,&converted,&size)) {
			free(converted);
			/*errno = KDB_ERR_CONVERT;*/
			return -1;
		}

		if (size>maxSize) {
			free(converted);
			/*errno=KDB_ERR_TOOLONG;*/
			return -1;
		}

		memcpy(relativeFilename,converted,size);
		free(converted);

		return size;
	} else {
		return kdbbKeyNameToRelativeFilename(keyName(key), relativeFilename, maxSize);
	}

	return -1;
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
ssize_t kdbbGetFullFilename(KDB *handle, const Key *forKey,char *returned,size_t maxSize) {
	ssize_t length=0;
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
	rc=kdbbKeyCalcRelativeFilename(forKey,returned+length,maxSize-length);

	if (rc == -1) return -1;
	else length += rc;

	return length;
}

