/***************************************************************************
            localkdb.c  -  Methods for accessing the Key Database
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the implementation of a filesystem backend for the            *
 *   Elektra Project. Each Key is a file in the filesystem.                *
 *   It is as secure as filesystem security. It is as reliable             *
 *   as filesystem. It uses only standards C calls, which makes it         *
 *   usable by very low level or early boot stage software, like           *
 *   /sbin/init.                                                           *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy$

*/


#include "kdb.h"
#include "kdbprivate.h"


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <iconv.h>
#include <locale.h>
#include <langinfo.h>
#include <ctype.h>
#include <string.h>


#define UTF8_TO   1
#define UTF8_FROM 0

extern int errno;




/**
 * @defgroup kdb KeyDB Class Methods
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 */


/*
 * @defgroup internals Elektra internals
 * @brief These methods are not to be used by your application.
 *
 */



/**
 * Opens a session with the Key database
 *
 * You should allways call this method before retrieving or commiting any
 * keys to the database. Otherwise, consequences are unpredictable.
 *
 * To simply manipulate Key or KeySet objects, you don't need to open the key
 * database before with this method.
 * @see kdbClose()
 * @ingroup kdb
 */
int kdbOpen() {
	/* load the environment and make us aware of codeset conversions */
	setlocale(LC_ALL,"");
	
	return 0;
}

/**
 * Closes a session with the Key database.
 *
 * You should call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects after kdbClose().
 *
 * This is the counterpart of kdbOpen().
 * @see kdbOpen()
 * @ingroup kdb
 */
int kdbClose() {
	return 0;
}

/**
 * Returns the size of the given key, once it is serialized.
 *
 * This call gives you a preview of the amount of memory required to
 * store the given key in its serialized form. Every field is taken into
 * account, including comments.
 * @param key the key which serialized size is to be calculated.
 * @return the serialized size in bytes.
 * @ingroup internals
 */
size_t keyGetSerializedSize(const Key *key) {
	size_t size,tmp;


	size=5+sizeof(u_int8_t)+1; /* RG000\nT\n */
	if (key->comment) size+=strblen(key->comment);
	size++;
	tmp=keyGetDataSize(key);
	size+=tmp;
	return size;
}

/**
 * Unencodes a buffer of hexadecimal values.
 *
 * <b>internal usage only-</b>
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * @param encoded the source of ASCII hexadecimal digits.
 * @param returned the destination for the unencoded data.
 * @return the amount of bytes unencoded.
 * @ingroup internals
 */
size_t unencode(char *encoded,void *returned) {
	char byteInHexa[5]="0x";
	char *readCursor=encoded;
	char *writeCursor=returned;

	if (!encoded) {
		if (returned) *(char *)returned=0;
		return 0;
	}

	byteInHexa[4]=0;
	while (*readCursor) {
		if (isspace((int)*readCursor)) readCursor++;
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
			return 0;
		}
	}
	return (long int)writeCursor-(long int)returned;
}

/**
 *
 * <b>internal usage only-</b>
 * @ingroup internals
 *
 */
int kdbNeedsUTF8Conversion() {
	return strcmp(nl_langinfo(CODESET),"UTF-8");
}


/**
 * Converts string to (direction=UTF8_TO) and from
 * (direction=UTF8_FROM) UTF-8.
 *
 * <b>internal usage only-</b>
 * @ingroup internals
 * @return 0 on success, -1 otherwise, and propagate @p errno
 *
 */
int UTF8Engine(int direction, char **string, size_t *inputByteSize) {
	char *currentCharset=0;
	char *converted=0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;

	if (kdbNeedsUTF8Conversion()) currentCharset=nl_langinfo(CODESET);
	else return 0;

	if (direction==UTF8_TO) converter=iconv_open("UTF-8",currentCharset);
	else converter=iconv_open(currentCharset,"UTF-8");

	if (converter == (iconv_t)(-1)) return -1;

	/* work with worst case, when all chars are wide */
	bufferSize=*inputByteSize * 4;
	converted=malloc(bufferSize);
	if (!converted) return -1;

	readCursor=*string;
	writeCursor=converted;
	if (iconv(converter,
			&readCursor,inputByteSize,
			&writeCursor,&bufferSize) == (size_t)(-1)) {
		free(converted);
		iconv_close(converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputByteSize=writeCursor-converted;
	/* store the current unencoded string for future free */
	readCursor=*string;
	/* allocate an optimal size area to store the converted string */
	*string=malloc(*inputByteSize);
	/* copy all that matters for returning */
	memcpy(*string,converted,*inputByteSize);
	/* release memory used by passed string */
	free(readCursor);
	/* release buffer memory */
	free(converted);
	/* release the conversor engine */
	iconv_close(converter);

	return 0;
}




/**
 *
 * <b>internal usage only-</b>
 * @ingroup internals
 *
 */
int handleOldKeyFileVersion(Key *key,FILE *input,u_int16_t nversion) {
	char generalBuffer[100];
	size_t currentBufferSize;

	char type[5];
	char *data=0;
	size_t dataSize=0;
	char *comment=0;
	size_t commentSize=0;

	int readComment=1;
	int eof=0;

	/*
		This is a very dirty helper.
		It has the parsing code for old version of key files.
		If your editor doesn't have code folding it will be a pain.
	*/


	switch (nversion) {
		case 1: {
			if (!fgets(type,    sizeof(type),    input)) return -1;

			while (readComment) {
				if (fgets(generalBuffer,sizeof(generalBuffer),input)) {
					if (memcmp(generalBuffer,"<DATA>\n\0",8)) {
						/* This is not the begining of the data part so it is part of comment */
						currentBufferSize=strblen(generalBuffer);
						if (!comment) {
							comment=(char *)malloc(commentSize=currentBufferSize);
							strcpy(comment,generalBuffer);
						} else {
							char *buffer=0;

							buffer=malloc(commentSize+currentBufferSize);
							strcpy(buffer,comment);
							strcat(buffer,generalBuffer);
							comment=realloc(comment,commentSize+=currentBufferSize);
							strcpy(comment,buffer);
							free(buffer);
						}
					} else readComment=0;
				} else {
					readComment=0;
					eof=1;
				}
			}

			/* Remove last \n */
			if (commentSize > 1 && (*(comment+commentSize-2) == '\n')) {
				*(comment+commentSize-2)=0;
				--commentSize;
			}


			if (comment && UTF8Engine(UTF8_FROM,&comment,&commentSize)) {
				free(comment);
				return -1;
			}

			/* Now read the data section */
			if (!eof) {
				while (fgets(generalBuffer,sizeof(generalBuffer),input)) {
					currentBufferSize=strlen(generalBuffer);
					if (!data) {
						data=(char *)malloc(dataSize=(currentBufferSize+1));
						strcpy(data,generalBuffer);
					} else {
						char *buffer=0;

						buffer=malloc(dataSize+currentBufferSize);
						strcpy(buffer,data);
						strcat(buffer,generalBuffer);
						data=realloc(data,dataSize+=currentBufferSize);
						strcpy(data,buffer);
						free(buffer);
					}
				}
			}

			/* Put in the Key object */
			keySetComment(key,comment);
			if (comment) free(comment);

			/* This is what changed from version 1 to
			   version 2 format: key type numbers */
			{
				u_int8_t oldVersion=atoi(type);
				switch (oldVersion) {
					case 1: keySetType(key,KEY_TYPE_BINARY); break;
					case 2: keySetType(key,KEY_TYPE_STRING); break;
					default: keySetType(key,oldVersion);
				}
			}
			if (!dataSize) {
				keySetRaw(key,0,0);
				return 0;
			}

			if (keyGetType(key) <= KEY_TYPE_BINARY) {
				/* Binary data. Unencode. */
				char *unencoded=0;
				size_t unencodedSize;

				/* raw data is maximum half the size of text-encoded data */
				unencodedSize=dataSize/2;

				unencoded=malloc(unencodedSize);
				if (!(unencodedSize=unencode(data,unencoded))) return -1;
				keySetRaw(key,unencoded,unencodedSize);
				free(unencoded);
			} else {
				if (UTF8Engine(UTF8_FROM,&data,&dataSize)) {
					free(data);
					return -1;
				}
				keySetRaw(key,data,dataSize);
			}

			free(data);

			return 0;
		} /* version 1 */
	} /* switch */
	return -1;
}





/**
 * Makes a key object from its serialized form, coming from a file.
 *
 * @param key the key we want to receive the data.
 * @param input the opened file from which we want to read.
 * @return 0 on success.
 * @ingroup internals
 */
int keyFileUnserialize(Key *key,FILE *input) {
	char generalBuffer[100];
	size_t currentBufferSize;

	char version[10];
	u_int16_t nversion=0;
	char type[5];
	char *data=0;
	size_t dataSize=0;
	char *comment=0;
	size_t commentSize=0;

	int readComment=1;
	int eof=0;

	/* The serialized format is
	   -------------------------
	   RG001\n
	   type\n
	   comment (with newlines)\n
	   <DATA>\n
	   The data encoded as text
	   -------------------------
	*/

	if (!fgets(version, sizeof(version), input)) return -1;
	if (strncmp(version,"RG",2)) {
		/* Doesn't look like a key file */
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}

	nversion=atoi(version+2);
	if (!nversion || nversion > RG_KEY_FORMAT_VERSION) {
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}

	if (nversion != RG_KEY_FORMAT_VERSION)
		return handleOldKeyFileVersion(key,input,nversion);

	if (!fgets(type,    sizeof(type),    input)) return -1;

	while (readComment) {
		if (fgets(generalBuffer,sizeof(generalBuffer),input)) {
			if (memcmp(generalBuffer,"<DATA>\n\0",8)) {
				/* This is not the begining of the data part so it is part of comment */
				currentBufferSize=strblen(generalBuffer);
				if (!comment) {
					comment=(char *)malloc(commentSize=currentBufferSize);
					strcpy(comment,generalBuffer);
				} else {
					char *buffer=0;

					buffer=malloc(commentSize+currentBufferSize);
					strcpy(buffer,comment);
					strcat(buffer,generalBuffer);
					comment=realloc(comment,commentSize+=currentBufferSize);
					strcpy(comment,buffer);
					free(buffer);
				}
			} else readComment=0;
		} else {
			readComment=0;
			eof=1;
		}
	}

	/* Remove last \n */
	if (commentSize > 1 && (*(comment+commentSize-2) == '\n')) {
		*(comment+commentSize-2)=0;
		--commentSize;
	}

	if (comment && UTF8Engine(UTF8_FROM,&comment,&commentSize)) {
		free(comment);
		return -1;
	}

	/* Now read the data section */
	if (!eof) {
		while (fgets(generalBuffer,sizeof(generalBuffer),input)) {
			currentBufferSize=strlen(generalBuffer);
			if (!data) {
				data=(char *)malloc(dataSize=(currentBufferSize+1));
				strcpy(data,generalBuffer);
			} else {
				char *buffer=0;

				buffer=malloc(dataSize+currentBufferSize);
				strcpy(buffer,data);
				strcat(buffer,generalBuffer);
				data=realloc(data,dataSize+=currentBufferSize);
				strcpy(data,buffer);
				free(buffer);
			}
		}
	}

	/* Put in the Key object */
	keySetComment(key,comment);
	if (comment) free(comment);
	keySetType(key,atoi(type));
	if (!dataSize) {
		keySetRaw(key,0,0);
		return 0;
	}

	if (keyGetType(key) <= KEY_TYPE_BINARY) {
		/* Binary data. Unencode. */
		char *unencoded=0;
		size_t unencodedSize;

		/* raw data is maximum half the size of text-encoded data */
		unencodedSize=dataSize/2;

		unencoded=malloc(unencodedSize);
		if (!(unencodedSize=unencode(data,unencoded))) return -1;
		keySetRaw(key,unencoded,unencodedSize);
		free(unencoded);
	} else {
		if (UTF8Engine(UTF8_FROM,&data,&dataSize)) {
			free(data);
			return -1;
		}
		keySetRaw(key,data,dataSize);
	}

	free(data);

	return 0;
}



/**
 * Encodes a buffer of data onto hexadecimal ASCII.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * unencode().
 *
 * @param unencoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the destination for the ASCII-encoded data.
 * @return the amount of bytes used in the resulting encoded buffer.
 * @see encode()
 * @ingroup internals
 */
size_t encode(void *unencoded, size_t size, char *returned) {
	void *readCursor=unencoded;
	void *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;

	while ((readCursor-unencoded)<size) {
		sprintf(writeCursor,"%01x",*(u_int8_t *)readCursor);
		readCursor++;
		writeCursor+=2;
		currentInBlock++;
		currentInLine++;
		if (currentInLine==lineStep) {
			*(char *)writeCursor='\n'; writeCursor++;
			currentInLine=0;
			currentInBlock=0;
		}
		if (currentInBlock==blockStep) {
			*(char *)writeCursor=' '; writeCursor++;
			currentInBlock=0;
		}
	}
	*(char *)writeCursor='\n';
	*(char *)++writeCursor=0;
	return (writeCursor)-(void *)returned;
}





/**
 * Writes the serialized form of the given key onto a file.
 *
 * This is the counterpart of <i>keyFileUnserialize()</i>.
 * @param key the key we want to serialize.
 * @param output the opened file to be written.
 * @return 0 on success.
 * @see keyFileUnserialize()
 * @ingroup internals
 */
int keyFileSerialize(Key *key, FILE *output) {
	/* The serialized format is
	   -------------------------
	   RG001\n
	   type\n
	   comment (with newlines)\n
	   <DATA>\n
	   The data encoded as text
	   -------------------------
	*/

	size_t dataSize;

	fprintf(output,"RG%03d\n",RG_KEY_FORMAT_VERSION);
	fprintf(output,"%d\n",key->type);
	if (key->comment) {
		if (kdbNeedsUTF8Conversion()) {
			size_t convertedCommentSize=key->commentSize;
			char *convertedComment=malloc(convertedCommentSize);

			memcpy(convertedComment,key->comment,key->commentSize);
			if (UTF8Engine(UTF8_TO,&convertedComment,&convertedCommentSize)) {
				free(convertedComment);
				return -1;
			}
			fprintf(output,"%s\n",convertedComment);
			free(convertedComment);
		} else fprintf(output,"%s\n",key->comment);
	}

	fputs("<DATA>\n",output);
	fflush(output);

	dataSize=keyGetDataSize(key);
	if (dataSize) {
		/* There is some data to write */
		if (keyGetType(key) > KEY_TYPE_BINARY) {
			if (kdbNeedsUTF8Conversion()) {
				size_t convertedDataSize=key->dataSize;
				char *convertedData=malloc(convertedDataSize);

				memcpy(convertedData,key->data,key->dataSize);
				if (UTF8Engine(UTF8_TO,&convertedData,&convertedDataSize)) {
					free(convertedData);
					return -1;
				}
				fprintf(output,"%s",convertedData);
				free(convertedData);
			} else fputs(key->data,output);
		} else {
			char *encoded=malloc(3*dataSize);
			size_t encodedSize;

			encodedSize=encode(key->data,dataSize,encoded);
			fwrite(encoded,encodedSize,1,output);
			free(encoded);
		}
	}
	return 0;
}


/**
 * This is a helper to kdbGetFilename()
 *
 * @param relativeFileName the buffer to return the calculated filename
 * @param maxSize maximum number of bytes that fit the buffer
 * @see kdbGetFilename()
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup internals
 */
size_t keyCalcRelativeFileName(Key *key,char *relativeFileName,size_t maxSize) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

// 	cursor=key->key;
// 	while (*cursor) {
// 		if (pos+1 > maxSize) {
// 			errno=E2BIG;
// 			return -1;
// 		}
// 		switch (*cursor) {
// 			case '\\':
// 				cursor++;
// 				relativeFileName[pos]=*cursor;
// 				break;
// 			case '.':
// 				relativeFileName[pos]='/';
// 				break;
// 			default:
// 				relativeFileName[pos]=*cursor;
// 		}
// 		cursor++;
// 		pos++;
// 	}
// 	relativeFileName[pos]=0;
// 	pos++;

	if (kdbNeedsUTF8Conversion()) {
		char *converted;
		size_t size;

		if (!(size=keyGetNameSize(key))) return 0;

		converted=malloc(size);
		keyGetName(key,converted,size);

// 		memcpy(converted,relativeFileName,convertedSize);

		if (UTF8Engine(UTF8_TO,&converted,&size)) {
			free(converted);
			return 0;
		}

		if (size>maxSize) {
			free(converted);
			errno=E2BIG;
			return 0;
		}

		memcpy(relativeFileName,converted,size);
		free(converted);

		return size;
	} else return keyGetName(key,relativeFileName,maxSize);

	return 0;
}






/**
 * Stats a key file.
 * Will not open the key file, but only stat it, not changing its last
 * access time.
 * The resulting key will have all info, but comment, value and value type.
 *
 * @param stat the stat structure to get metadata from
 * @param key object to be filled with info from stat structure
 * @return 0 on success, -1 otherwise
 * @ingroup internals
 */
int keyFromStat(Key *key,struct stat *stat) {
	if (!key) {
		errno=KDB_RET_NULLKEY;
		return -1;
	}

	keySetAccess(key,stat->st_mode);
	keySetUID(key,stat->st_uid);
	keySetGID(key,stat->st_gid);
	if (S_ISDIR(stat->st_mode)) keySetType(key,KEY_TYPE_DIR);
	key->atime=stat->st_atime;
	key->mtime=stat->st_mtime;
	key->ctime=stat->st_ctime;
	key->recordSize=stat->st_size;
	return 0;
}




/**
 * Calculate the real file name for a key.
 *
 * @param returned the buffer to return the calculated filename
 * @param maxSize maximum number of bytes that fit the buffer
 * @see kdbCalcRelativeFilename()
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup internals
 */
size_t kdbGetFilename(Key *forKey,char *returned,size_t maxSize) {
	size_t length=0;

	switch (keyGetNamespace(forKey)) {
		case KEY_NS_SYSTEM: {
			/* Prepare to use the 'system/ *' database */
			strncpy(returned,RG_DB_SYSTEM,maxSize);
			length=strlen(returned);
			break;
		}
		case KEY_NS_USER: {
			/* Prepare to use the 'user:????/ *' database */
			struct passwd *user=0;
			char userName[100];

			length=keyGetOwner(forKey,userName,sizeof(userName));
			if (!length) strncpy(userName,getenv("USER"),sizeof(userName));

			user=getpwnam(userName);
                        if (!user) return 0; /* propagate errno */
			length=snprintf(returned,maxSize,"%s/%s",user->pw_dir,RG_DB_USER);
			break;
		}
		default: {
			errno=KDB_RET_INVALIDKEY;
			return 0;
		}
	}

	returned[length]='/'; length++;
	length+=keyCalcRelativeFileName(forKey,returned+length,maxSize-length);

	return length;
}



/**
 * A high-level method to get a key value, by key name.
 * This method is valid only for string keys.
 * You should use other methods to get non-string keys.
 *
 * @see kdbSetValue()
 * @see kdbGetKey()
 * @see kdbGetValueByParent()
 * @see keyGetString()
 * @return 0 on success, or other value and errno is set
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @ingroup kdb
 *
 */
int kdbGetValue(const char *keyname, char *returned,size_t maxSize) {
	Key key;
	int rc=0;

	keyInit(&key);
	keySetName(&key,keyname);
	rc=kdbGetKey(&key);
	if (rc == 0) keyGetString(&key,returned,maxSize);
	else rc=errno; /* store errno before a possible change */
	keyClose(&key);
	errno=rc;
	return rc;
}



/**
 * A high-level method to set a value to a key, by key name.
 * It will obviously check if key exists first, and keep its metadata.
 * So you'll not loose the precious key comment.
 *
 * This will set a text key. So if the key was previously a binary, etc key, it will be retyped as text.
 *
 * @see kdbGetValue()
 * @see keySetString()
 * @see kdbSetKey()
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success, other value otherwise, and errno is set
 * @ingroup kdb
 */
int kdbSetValue(const char *keyname, const char *value) {
	Key key;
	int rc;

/* TODO: check key type first */
	keySetName(&key,keyname);
	rc=kdbGetKey(&key);
	keySetString(&key,value);
	rc=kdbSetKey(&key);
	keyClose(&key);
	return rc;
}



/**
 * Fills the \p returned buffer with the value of a key, which name
 * is the concatenation of \p parentName and \p baseName.
 *
 * @par Example:
 * @code
char *parent="user/sw/MyApp";
char *keys[]={"key1","key2","key3"};
char buffer[150];   // a big buffer
int c;

for (c=0; c<3; c++) {
	kdbGetValueByParent(parent,keys[c],buffer,sizeof(buffer));
	// Do something with buffer....
}

 * @endcode
 *
 * @see kdbGetKeyByParent()
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param returned pre-allocated buffer to be filled with key value
 * @param maxSize size of the \p returned buffer
 * @return whathever is returned by kdbGetValue()
 * @ingroup kdb
 */
int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned, size_t maxSize) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	return kdbGetValue(name,returned,maxSize);
}



/**
 * Sets the provided @p value to the key whose name is the concatenation of
 * @p parentName and @p baseName.
 *
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param value the value to set
 * @ingroup kdb
 */
int kdbSetValueByParent(const char *parentName, const char *baseName, const char *value) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	return kdbSetValue(name,value);
}



/**
 * Given a parent key name plus a basename, returns the key.
 *
 * So here you'll provide something like
 * - @p system/sw/myApp plus @p key1 to get @p system/sw/myApp/key1
 * - @p user/sw/MyApp plus @p dir1/key2 to get @p user/sw/MyApp/dir1/key2
 *
 * @param parentName parent key name
 * @param baseName leaf or child name
 * @param returned a pointer to an initialized key to be filled
 * @return 0 on success, or what kdbGetKey() returns, and errno is set
 * @see kdbGetKey()
 * @see kdbGetValueByParent()
 * @see kdbGetKeyByParentKey()
 * @ingroup kdb
 */
int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	keySetName(returned,name);

	return kdbGetKey(returned);
}


/**
 * Similar to previous, provided for convenience.
 * @param parent pointer to the parent key
 * @see kdbGetKey()
 * @see kdbGetKeyByParent()
 * @see kdbGetValueByParent()
 * @return 0 on success, or what kdbGetKey() returns, and errno is set
 * @ingroup kdb
 */
int kdbGetKeyByParentKey(const Key *parent, const char *baseName, Key *returned) {
	size_t size=keyGetFullNameSize(parent);
	char name[size+strblen(baseName)];

	keyGetFullName(parent,name,size);
	name[size-1]='/';
	strcpy((char *)(parent+size),baseName);

	keySetName(returned,name);

	return kdbGetKey(returned);
}




/* Used by the qsort() function */
int keyCompareByName(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;

	return strcmp(key1->key, key2->key);
}



/**
 * Retrieve a number of inter-related keys in one shot.
 * This is one of the most practicall methods of the library.
 * Returns a KeySet with all retrieved keys. So if your application keys
 * live bellow @p system/sw/myApp, you'll use this method to get them all.
 *
 * Option can be any of the following, ORed:
 * - @p KDB_O_RECURSIVE \n
 *   Retrieve also the keys under the child keys, recursively.
 *   The kdb(1) ls command, with switch -R uses this option.
 * - @p KDB_O_DIR \n
 *   By default, folder keys will not be returned because they don't have
 *   values and exist only to define hierarchy. Use this option if you need
 *   them to be included in the returned KeySet.
 * - @p KDB_O_NOVALUE \n
 *   Do not include in @p returned the regular value keys. The resulting KeySet
 *   will be only the skeleton of the tree.
 * - @p KDB_O_STATONLY \n
 *   Only stat(2) the keys; do not retrieve the value, comment and key data
 *   type. The resulting keys will be empty and usefull only for
 *   informational purposes. The kdb(1) ls command, without the -v switch
 *   uses this option.
 * - @p KDB_O_INACTIVE \n
 *   Will make it not ignore inactive keys. So @p returned will be filled also
 *   with inactive keys. See registry(7) to understand how inactive keys work.
 * - @p KDB_O_SORT \n
 *   Will sort keys alphabetically by their names.
 *
 * @par Example:
 * @code
KeySet myConfig;
ksInit(&myConfig);

kdbOpen();
rc=kdbGetChildKeys("system/sw/MyApp", &myConfig, KDB_O_RECURSIVE);
kdbClose();

// Check and handle propagated error
if (rc) switch (errno) {
	case....
	case...
}

ksRewind(&myConfig); // go to begining of KeySet
Key *key=ksNext(&myConfig);
while (key) {
	// do something with key . . .

	key=ksNext(&myConfig)); // next key
}
 * @endcode
 *
 * @param parentName name of the parent key
 * @param returned the KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @see KDBOptions
 * @see commandList() code in kdb command for usage example
 * @see commandEdit() code in kdb command for usage example
 * @see commandExport() code in kdb command for usage example
 * @return 0 on success, other value on error and @c errno is set
 * @ingroup kdb
 *
 */
int kdbGetChildKeys(const char *parentName, KeySet *returned, unsigned long options) {
	char *realParentName=0;
	size_t parentNameSize;
	DIR *parentDir;
	Key parentKey;
	char buffer[800];
	struct dirent *entry;

	/*
		- Convert parent key name into a real filename
		- Check if it is a directory. Open it
		- Browse, read and include in the KeySet
	*/
	keyInit(&parentKey);
	keySetName(&parentKey,parentName);
	kdbGetFilename(&parentKey,buffer,sizeof(buffer));
	parentDir=opendir(buffer);

	/* Check if Key is not a directory or doesn't exist.
	 * Propagate errno */
	if (!parentDir) return -1;

	parentNameSize=keyGetFullNameSize(&parentKey);
	realParentName=realloc(realParentName,parentNameSize);
	keyGetFullName(&parentKey,realParentName,parentNameSize);

	while ((entry=readdir(parentDir))) {
		Key *keyEntry;
		char *transformedName=0;
		size_t keyNameSize=0;

		/* Ignore '.' and '..' directory entries */
		if (!strcmp(entry->d_name,".") || !strcmp(entry->d_name,"..")) continue;

		/* If key name starts with '.', and don't want INACTIVE keys, ignore it */
		if ((*entry->d_name == '.') && !(options & KDB_O_INACTIVE)) continue;

		/* Next 2 ifs are required to transform filename from UTF-8 */
		if (!transformedName) {
			transformedName=
				realloc(transformedName,keyNameSize=strblen(entry->d_name));
			strcpy(transformedName,entry->d_name);
		}
		if (UTF8Engine(UTF8_FROM,&transformedName,&keyNameSize)) {
			free(transformedName);
			return -1;
		}

		/* Copy the entire transformed key name to our final buffer */
		sprintf(buffer,"%s/%s",realParentName,transformedName);
		free(transformedName); /* don't need it anymore */

		keyEntry=(Key *)malloc(sizeof(Key)); keyInit(keyEntry);
		keySetName(keyEntry,buffer);

		if (options & KDB_O_STATONLY) kdbStatKey(keyEntry);
		else if (options & KDB_O_NFOLLOWLINK) {
			kdbStatKey(keyEntry);
			if (!keyIsLink(keyEntry)) kdbGetKey(keyEntry);
		} else {
			int rc=kdbGetKey(keyEntry);
			/* If this is a permission problem, at least stat the key */
			if (rc && errno==KDB_RET_NOCRED) kdbStatKey(keyEntry);
		}

		if (S_ISDIR(keyEntry->access)) {
			if (options & KDB_O_RECURSIVE) {
				KeySet children;
				char *fullName;
				size_t fullNameSize;

				fullName=malloc(fullNameSize=keyGetFullNameSize(keyEntry));
				keyGetFullName(keyEntry,fullName,fullNameSize);

				ksInit(&children);
				/* Act recursively, without sorting. Sort in the end, once */
				kdbGetChildKeys(fullName,&children, ~(KDB_O_SORT) & options);

				/* Insert the current directory key in the returned list before its children */
				if (options & KDB_O_DIR) ksAppend(returned,keyEntry);
				else {
					keyClose(keyEntry);
					free(keyEntry);
				}

				/* Insert the children */
				ksAppendKeys(returned,&children);
				free(fullName);
			} else if (options & KDB_O_DIR) ksAppend(returned,keyEntry);
				else {
					keyClose(keyEntry);
					free(keyEntry);
				}
		} else if (options & KDB_O_NOVALUE) {
			keyClose(keyEntry);
			free(keyEntry);
		} else ksAppend(returned,keyEntry);
	} /* while(readdir) */
	keyClose(&parentKey);
	free(realParentName);

	if ((options & (KDB_O_SORT)) && (returned->size > 1)) {
		Key *keys[returned->size];
		Key *cursor;
		size_t c=0;

		for (cursor=returned->start; cursor; cursor=cursor->next, c++)
			keys[c]=cursor;

		qsort(keys,returned->size,sizeof(Key *),keyCompareByName);

		returned->start=cursor=keys[0];
		for (c=1; c<returned->size; c++) {
			cursor->next=keys[c];
			cursor=cursor->next;
		}
		cursor->next=0;
		returned->end=cursor;
	}

	return 0;
}








/**
 * Returns a KeySet with all root keys currently recognized.
 * Currently, the @p system and current user's @p user keys are returned.
 * @param returned the initialized KeySet to be filled
 * @return 0
 * @see KeyNamespace
 * @see commandList() code in kdb command for usage example
 * @ingroup kdb
 *
 */
int kdbGetRootKeys(KeySet *returned) {
	Key *system=0,*user=0;

	system=malloc(sizeof(Key));
	keyInit(system);
	keySetName(system,"system");
	if (kdbGetKey(system)) {
		free(system);
		system=0;
	}

	user=malloc(sizeof(Key));
	keyInit(user);
	keySetName(user,"user");
	if (kdbGetKey(user)) {
		free(user);
		user=0;
	}

	if (system) ksInsert(returned,system);
	if (user) ksAppend(returned,user);

	return 0;
}



/**
 * Retrieves only the meta-info of a key from backend storage.
 * The bahavior may change from backend to backend. In the filesystem
 * backend, it will make only a stat to the key.
 *
 * Info like comments and key data type are not retrieved.
 *
 * @param key an initialized Key pointer to be filled.
 * @return 0 on success, -1 otherwise
 * @ingroup kdb
 */
int kdbStatKey(Key *key) {
	char keyFileName[800];
	struct stat keyFileNameInfo;
	size_t pos;
	u_int32_t semiflag;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* something is wrong */

	if (lstat(keyFileName,&keyFileNameInfo)) return -1;
	keyFromStat(key,&keyFileNameInfo);

	if (keyIsLink(key) && key->recordSize) {
		char *data=malloc(key->recordSize+1); /* Add 1 byte for ending 0 */

		readlink(keyFileName,data,key->recordSize);
		data[key->recordSize]=0; /* null terminate it */
		keySetLink(key,data);
		free(data);
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;
	key->flags |= KEY_FLAG_ACTIVE;

	return 0;
}



/**
 * Fully retrieves the passed @p key from the backend storage.
 * @param key a pointer to a Key that has a name set
 * @return 0 on success, or other value and @p errno is set
 * @see kdbSetKey()
 * @see commandGet() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbGetKey(Key *key) {
	char keyFileName[500];
	struct stat keyFileNameInfo;
	int fd;
	size_t pos;
	u_int32_t semiflag;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* something is wrong */

	if ((fd=open(keyFileName,O_RDONLY))==-1) return -1;
	/* TODO: lock at this point */
	fstat(fd,&keyFileNameInfo);
	keyFromStat(key,&keyFileNameInfo);
	if (!keyIsDir(key)) {
		FILE *input;

		input=fdopen(fd,"r");
		if (keyFileUnserialize(key,input)) {
			fclose(input);
			return -1;
		}
		/* TODO: unlock at this point */
		fclose(input);
	} else close(fd);

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}



/**
 * Commits an entire KeySet to the backend storage.
 * Each key is checked with keyNeedsSync() before being actually commited. So
 * only changed keys are updated.
 *
 * @param ks a KeySet full of changed keys
 * @return 0 (no way to know if some key failled currently)
 * @see kdbSetKey()
 * @see commandEdit() code in kdb command for usage example
 * @see commandLoad() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbSetKeys(KeySet *ks) {
	Key *current;

	for (current=ks->start; current; current=current->next) {
		if (keyNeedsSync(current)) {
			kdbSetKey(current);
		}
	}

	return 0;
}



/**
 * Commits a key to the backend storage.
 * If failed (see return), the @p errno global is set accordingly.
 *
 * @see kdbGetKey()
 * @see kdbSetKeys()
 * @see commandSet() code in kdb command for usage example
 * @return 0 on success, or other value and errno is set
 * @ingroup kdb
 */
int kdbSetKey(Key *key) {
	char keyFileName[800];
	char folderMaker[800];
	char *cursor, *last;
	int fd;
	FILE *output=0;
	size_t pos;
	u_int32_t semiflag;
	struct stat stated;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* Something is wrong. Propagate errno. */

	if (stat(keyFileName,&stated))
		if (errno==ENOENT) {
			/* check if parent dir already exists */
			last=rindex(keyFileName,'/');
			strncpy(folderMaker,keyFileName,last-keyFileName);
			folderMaker[last-keyFileName]=0;
			if (stat(folderMaker,&stated)) {
				/* create all path recursively until before our basename */
				last   =rindex(keyFileName,'/');
				cursor = index(keyFileName,'/'); cursor++; /* skip first occurence */
				if (!last || !cursor) { /* bizarre key name */
					errno=KDB_RET_INVALIDKEY;
					return -1;
				}
				for (cursor=index(cursor,'/');
						cursor && (cursor <= last);
						cursor=index(cursor,'/')) {
					strncpy(folderMaker,keyFileName,cursor-keyFileName);
					folderMaker[cursor-keyFileName]=0;
					if (mkdir(folderMaker,0775)<0 && errno!=EEXIST) return -1;
					cursor++;
				}
			}
		} else return -1; /* propagate errno */
	else { /* A file or dir or link is already there. Lets check details */
		/* TODO: Check for existing link */
		if ( S_ISDIR(stated.st_mode) && !keyIsDir(key)) {
			errno=EISDIR;
			return -1;
		}
		if (!S_ISDIR(stated.st_mode) &&  keyIsDir(key)) {
			errno=ENOTDIR;
			return -1;
		}
	}

	/* Enough of checking. Now real write stuff, with a bit other checks :-)  */

	if (keyIsLink(key)) {
		char *targetName=0;
		Key target;
		int rc;

		/*
			If targetName starts with:
			- "system" | "user" | any future root name: Convert to a FS path,
			  and symlink it
			- other: It is an absolute FS path, or relative inside-kdb
			  namespace path, and symlink it
		*/

		keyInit(&target);
		if (key->data) targetName=malloc(key->dataSize);
		keyGetLink(key,targetName,key->dataSize);

		/* Setting the name will let us know if this is a valid keyname */
		if (keySetName(&target,targetName)) {
			/* target has a valid key name */
			targetName=realloc(targetName,800);
			kdbGetFilename(&target,targetName,800);
			keyClose(&target);
		} else if (errno==KDB_RET_INVALIDKEY) {
			/* Is an invalid key name. So treat it as a regular file */
			keyClose(&target); /* get rid of invalid stuff */
		} else {
			keyClose(&target); /* get rid of invalid stuff */
			return -1; /* propagate errno from keySetName() */
		}


		/* Now, targetName has the real destination of our link */

		/* TODO: handle null targetName */
		rc=symlink(targetName,keyFileName);
		free(targetName);

		return rc; /* propagate errno */
	} else if (keyIsDir(key)) {
		if (mkdir(keyFileName,keyGetAccess(key))<0 &&
				errno!=EEXIST) return -1;
	} else {
		/* Try to open key file with its full file name */
		/* TODO: Make it more "transactional" without truncating */
		fd=open(keyFileName,O_CREAT | O_RDWR | O_TRUNC, keyGetAccess(key));
		/* TODO: lock file here */
		if (fd==-1) return -1;
		if (getuid() == 0) fchown(fd,keyGetUID(key),keyGetGID(key));
		if (!(output=fdopen(fd,"w+"))) return -1;
		if (keyFileSerialize(key,output)) {
			fclose(output);
			return -1;
		}
		/* TODO: unlock file here */
		fclose(output);
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}






/**
 * Remove a key from the backend storage.
 * This method is not recursive.
 *
 * @param keyName the name of the key to be removed
 * @return whathever is returned by remove(), and @p errno is propagated
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemove(const char *keyName) {
	Key key;
	char fileName[800];
	off_t rc;

	keyInit(&key);
	rc=keySetName(&key,keyName);
	if (rc==-1) return -1;
	rc=kdbGetFilename(&key,fileName,sizeof(fileName));
	keyClose(&key);
	if (!rc) return -1;

	return remove(fileName);
}





/**
 * Create a link key that points to other key.
 *
 * @param oldPath destination key name
 * @param newKeyName name of the key that will be created and will point
 * to @param oldPath
 * @return whathever is returned by kdbSetKey(), and \p errno is set
 * @see commandLink() code in kdb command for usage example
 * @see commandSet() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbLink(const char *oldPath, const char *newKeyName) {
	Key key;
	int rc;

	keyInit(&key);
	keySetName(&key,newKeyName);
	keySetLink(&key,oldPath);

	rc=kdbSetKey(&key);
	keyClose(&key);

	return rc;
}





/**
 * Monitor a KeySet for some key change.
 *
 * This method will scan the @p interests KeySet, starting and finishing in
 * the KeySet next cursor position, in a circular behavior, looking for some
 * change defined in the @p diffMask mask. It will use kdbMonitorKey()
 * and will return at the first key change ocurrence, or when requested
 * iterations finish.
 *
 * You may check the return code to see if some key changed, and get
 * the updated key using ksCurrent().
 *
 * @par Example:
 * @code
KeySet myConfigs;

ksInit(&myConfigs);
kdbGetChildKeys("system/sw/MyApp",&myConfigs,KDB_O_ALL);

// use the keys . . . .

// now monitor any key change
ksRewind(&myConfigs);
while (1) {
	Key *changed=0;
	char keyName[300];
	char keyData[300];
	u_int32_t diff;

	// block until any change in key value or comment . . .
	diff=kdbMonitorKeys(&myConfigs,
		KEY_FLAG_HASDATA | KEY_FLAG_HASCOMMENT,
		0,0); // ad-infinitum

	changed=ksCurrent(&myConfigs);
	keyGetName(changed,keyName,sizeof(keyName));

	switch (diff) {
		case KEY_FLAG_FLAG:
			printf("Key %s was deleted\n",keyName);
			break;
		case KEY_FLAG_NEEDSYNC:
			printf("No cretentials to access Key %s\n",keyName);
			break;
		default:
			keyGetString(changed,keyData,sizeof(keyData));
			printf("Key %s has changed its value to %s\n",keyName,keyData);
	}
}
 * @endcode
 *
 * @see kdbMonitorKey()
 * @see ksCurrent()
 * @see ksRewind()
 * @see ksNext()
 * @see KeyFlags
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
u_int32_t kdbMonitorKeys(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	Key *start,*current;
	u_int32_t diff;
	int infinitum=0;

	if (!interests || !interests->size) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleep) sleep=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	current=start=ksCurrent(interests);

	while (infinitum || --iterations) {
		do {
			diff=kdbMonitorKey(current,diffMask,1,0);
			if (diff) return diff;
			current=ksNext(interests);
		} while (current!=start);

		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleep);
	}
	return 0;
}



/**
 * Monitor a key change.
 *
 * This method will block your program until one of the folowing happens:
 * - All requested iterations, with requested sleep times, finish.
 *   If no change happens, zero is returned.
 * - Requested key info and meta-info (defined by @p diffMask) changes when
 *   keyCompare()ed with the original @p interest.
 *
 * @p interest should be a full key with name, value, comments, permissions,
 * etc, and all will be compared and then masked by @p diffMask.
 *
 * If @p interest is a folder key, use @c KEY_FLAG_HASTIME in @p diffMask
 * to detect a time change, so you'll know something happened (key
 * modification, creation, deletion) inside the folder.
 *
 * If @p interest was not found, or deleted, the method will return
 * immediatly a @c KEY_FLAG_FLAG value.
 *
 * If you don't have access rights to @p interest, the method will return
 * immediatly a @c KEY_FLAG_NEEDSYNC value.
 *
 * If something from @p diffMask has changed in @p interest, it will be
 * updated, so when method returns, you'll have an updated version of the key.
 *
 * @param interest key that will be monitored
 * @param diffMask what particular info change we are interested
 * @param iterations how many times to test, when 0 means until
 * some change happens
 * @param sleep time to sleep, in microseconds, between iterations.
 * 0 defaults to 1 second.
 * @return the ORed @p KEY_FLAG_* flags of what changed
 * @see KeyFlags
 * @see keyCompare()
 * @see kdbMonitorKeys() to monitor KeySets, and for a code example
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
u_int32_t kdbMonitorKey(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	Key tested;
	int rc;
	u_int32_t diff;
	int infinitum=0;

	/* consistency */
	if (!interest || !keyGetNameSize(interest)) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleep) sleep=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	/* Work with a copy of the key */
	keyInit(&tested);
	keyDup(interest,&tested);

	while (infinitum || --iterations) {
		rc=kdbGetKey(&tested);
		if (rc) {
			/* check what type of problem happened.... */
			switch (errno) {
				case KDB_RET_NOCRED:
					return KEY_FLAG_NEEDSYNC;
				case KDB_RET_NOTFOUND:
					return KEY_FLAG_FLAG;
			}
		}
		diff=keyCompare(&tested,interest);
		if (diff & diffMask) {
			/* If differences are in the diff mask...*/
			keyDup(&tested,interest);
			keyClose(&tested);
			return diff;
		}
		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleep);
	}

	return 0;
}



/**
 * @mainpage The Elektra Project API
 *
 * @section overview Elektra Project Overview
 *
 * Elektra is a project to unify Linux/Unix configurations. It does that
 * providing an hierarchical namespace to store configuration keys and
 * their values, an API to access/modify them, and some command line tools.
 *
 * Everything about the project can be found at http://elektra.sf.net
 *
 * @section classes Elektra API
 *
 * The API was written in pure C because Elektra was designed to be usefull
 * even for the most basic system programs, which are all made in C. Also,
 * being C, bindings to other languages can appear, as we already have for
 * Python, Ruby, etc.
 *
 * The API follows an Object Oriented design, and there are only 3 classes
 * as shown by the figure:
 *
 * @image html classes.png "Elektra Classes"
 *
 * Some general things you can do with each class are:
 *
 * @subsection KeyDB KeyDB
 *   - Retrieve and commit Keys and KeySets, recursively or not
 *   - Retrieve and commit individual Keys value, by absolute name or relative to parent
 *   - Monitor and notify changes in Keys and KeySets
 *   - Create and delete regular, folder or symbolic link Keys
 *
 * @subsection Key Key
 *   - Get and Set key properties like name, root and base name, value, type,
 *      permissions, changed time, description, etc
 *   - Compare all properties with other keys
 *   - Test if changed, if is a @p user/ or @p system/ key, etc
 *   - Flag it and test if key has a flag
 *   - Export Keys to an XML representation
 *
 * @subsection KeySet KeySet
 *   - Linked list of Key objects
 *   - Insert and append entire KeySets or Keys
 *   - Work with its internal cursor
 *   - Compare entire KeySets
 *   - Export KeySets to an XML representation
 *
 *
 * @section keynames Key Names and Namespaces
 *
 * There are 2 trees of keys: @p system and @p user
 *
 * @subsection systemtree The "system" Subtree
 *
 * It is provided to store system-wide configuration keys, that is,
 * configurations that daemons and system services will use.
 *
 * @subsection usertree The "user" Subtree
 *
 * Used to store user-specific configurations, like the personal settings
 * of a user to certains programs
 *
 *
 * @section rules Rules for Key Names
 *
 * When using Elektra to store your application's configuration and state,
 * please keep in mind the following rules:
 * - You are not allowed to create keys right under @p system or @p user.
 * - You are not allowed to create folder keys right under @p system or @p user.
 *   They are reserved for very essential OS subsystems.
 * - The keys for your application, called say @e MyApp, should be created under
 *   @p system/sw/MyApp and/or @p user/sw/MyApp.
 * - It is suggested to make your application look for default keys under
 *   @p system/sw/MyApp/current and/or @p user/sw/MyApp/current. This way, from
 *   a sysadmin perspective, it will be possible to copy the
 *   @p system/sw/MyApp/current tree to something like @p system/sw/MyApp/old,
 *   and keep system clean and organized.
 *
 *
 */








