/***************************************************************************
            registry.h  -  Methods for accessing the Linux Registry
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
 *   Linux Registry. Each Key is a file in the filesystem.                 *
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


#include "registry.h"
#include "registryprivate.h"


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
 * @defgroup registry Registry Access Methods
 * @brief These are general methods to access the Key database.
 * 
 */

 
/**
 * @defgroup internals Registry internals
 * @brief These methods are not to be used by your application.
 * 
 */

 
 
/**
 * Opens a registry session.
 * 
 * By now it does nothing. This might change in the future, so it's good
 * practice to always call <i>registryOpen()</i> before using the registry.
 * @see registryClose()
 * @ingroup registry
 */
int registryOpen() {
	return 0;
}

/**
 * Closes a registry session.
 *
 * This is the counterpart of <i>registryOpen()</i>.
 * @see registryOpen()
 * @ingroup registry
 */
int registryClose() {
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
size_t keyGetSerializedSize(Key *key) {
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
			errno=RG_KEY_RET_TYPEMISMATCH;
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
int registryNeedsUTF8Conversion() {
	setlocale(LC_ALL,"");
	return strcmp(nl_langinfo(CODESET),"UTF-8");
}


/** 
 * Converts string to (direction=UTF8_TO) and from (direction=UTF8_FROM) UTF-8.
 *
 * <b>internal usage only-</b>
 * @ingroup internals
 *
 */
int UTF8Engine(int direction, char **string, size_t *inputByteSize) {
	char *currentCharset=0;
	char *converted=0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;

	if (registryNeedsUTF8Conversion()) currentCharset=nl_langinfo(CODESET);
	else return 0;

	if (direction) converter=iconv_open("UTF-8",currentCharset);
	else converter=iconv_open(currentCharset,"UTF-8");

	if (converter == (iconv_t)(-1)) return -1;

	bufferSize=*inputByteSize*2; /* work with worst case */
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
					case 1: keySetType(key,RG_KEY_TYPE_BINARY); break;
					case 2: keySetType(key,RG_KEY_TYPE_STRING); break;
					default: keySetType(key,oldVersion);
				}
			}
			if (!dataSize) {
				keySetRaw(key,0,0);
				return 0;
			}

			if (keyGetType(key) <= RG_KEY_TYPE_BINARY) {
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

	/* TODO: Handle different format versions */

	if (!fgets(version, sizeof(version), input)) return -1;
	if (strncmp(version,"RG",2)) {
		/* Doesn't look like a key file */
		errno=RG_KEY_RET_INVALIDKEY;
		return -1;
	}
	
	nversion=atoi(version+2);
	if (!nversion || nversion > RG_KEY_FORMAT_VERSION) {
		errno=RG_KEY_RET_INVALIDKEY;
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

	if (keyGetType(key) <= RG_KEY_TYPE_BINARY) {
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
 * <b>internal usage only-</b>
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * <i>encode()</i>.
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
		if (registryNeedsUTF8Conversion()) {
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
		if (keyGetType(key) > RG_KEY_TYPE_BINARY) {
			if (registryNeedsUTF8Conversion()) {
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
 * This is a helper to registryGetFilename()
 * 
 * @param relativeFileName the buffer to return the calculated filename
 * @param maxSize maximum number of bytes that fit the buffer
 * @see registryGetFilename()
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup internals
 */
size_t keyCalcRelativeFileName(Key *key,char *relativeFileName,size_t maxSize) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}
	if (!key->key) {
		errno=RG_KEY_RET_NOKEY;
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

	if (registryNeedsUTF8Conversion()) {
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
 * Will not open the key file, but only stat it, not changing its last access time.
 * The resulting key will have all info, but comment, value and value type.
 *
 * @param stat the stat structure to get metadata from
 * @param key object to be filled with info from stat structure
 * @return 0 on success, -1 otherwise
 * @ingroup internals
 */
int keyFromStat(Key *key,struct stat *stat) {
	if (!key) {
		errno=RG_KEY_RET_NULLKEY;
		return -1;
	}

	keySetAccess(key,stat->st_mode);
	keySetUID(key,stat->st_uid);
	keySetGID(key,stat->st_gid);
	if (S_ISDIR(stat->st_mode)) keySetType(key,RG_KEY_TYPE_DIR);
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
 * @see registryCalcRelativeFilename()
 * @return number of bytes written to the buffer, or 0 on error
 * @ingroup internals
 */
size_t registryGetFilename(Key *forKey,char *returned,size_t maxSize) {
	size_t length=0;

	switch (keyGetNameSpace(forKey)) {
		case RG_NS_SYSTEM: {
			/* Prepare to use the 'system/ *' database */
			strncpy(returned,RG_DB_SYSTEM,maxSize);
			length=strlen(returned);
			break;
		}
		case RG_NS_USER: {
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
			errno=RG_KEY_RET_INVALIDKEY;
			return 0;
		}
	}

	returned[length]='/'; length++;
	length+=keyCalcRelativeFileName(forKey,returned+length,maxSize-length);

	return length;
}


/**
 * A high-level method to set a value to a key, by key name.
 * It will obviously check if key exists first, and keep its metadata.
 * So you'll not loose the precious key comment.
 * 
 * This will set a text key. So if the key was previously a binary, etc key, it will be retyped as text.
 *
 * @see registryGetValue()
 * @see keySetString()
 * @see registrySetKey()
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success, and error code otherwise
 * @ingroup registry
 */
int registrySetValue(char *keyname, char *value) {
	Key key;
	int rc;
	
/* TODO: check key type first */
	keySetName(&key,keyname);
	rc=registryGetKey(&key);
	keySetString(&key,value);
	rc=registrySetKey(&key);
	keyClose(&key);
	return rc;
}



/**
 * A high-level method to get a key value, by key name.
 * This method is valid only for string keys.
 * You should use other methods to get non-string keys.
 *
 * @see registrySetValue()
 * @see registryGetKey()
 * @see keyGetString()
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @ingroup registry
 *
 */
int registryGetValue(char *keyname,char *returned,size_t maxSize) {
	Key key;
	int rc=0;

	keyInit(&key);
	keySetName(&key,keyname);
	rc=registryGetKey(&key);
	if (rc == 0) keyGetString(&key,returned,maxSize);
	else rc=errno; /* store errno before a possible change */
	keyClose(&key);
	errno=rc;
	return rc;
}



/**
 * Given a parent key name plus a basename, returns the key.
 *
 * So here you'll provide something like
 * - system/sw/myApp plus key1 to get system/sw/myApp/key1
 * - user/sw/MyApp plus dir1/key2 to get user/sw/MyApp/dir1/key2
 *
 * @param parentName parent key name
 * @param baseName leaf name
 * @param returned the returned key
 * @return 0 on success, or a return code if key was not retrieved for some reason
 * @see registryGetKey()
 * @see registryGetValueByParent()
 * @ingroup registry
 */
int registryGetKeyByParent(char *parentName, char *baseName, Key *returned) {
	char name[strblen(parentName)+strblen(baseName)];
	
	sprintf(name,"%s/%s",parentName,baseName);
	keySetName(returned,name);
	
	return registryGetKey(returned);
}


/**
 * @ingroup registry
 */
int registryGetKeyByParentKey(Key *parent, char *baseName, Key *returned) {
	size_t size=keyGetFullNameSize(parent);
	char name[size+strblen(baseName)];
	
	keyGetFullName(parent,name,size);
	name[size-1]='/';
	strcpy((char *)(parent+size),baseName);

	keySetName(returned,name);
	
	return registryGetKey(returned);
}


/**
 * @ingroup registry
 */
int registryGetValueByParent(char *parentName, char *baseName, char *returned, size_t maxSize) {
	char name[strblen(parentName)+strblen(baseName)];
	
	sprintf(name,"%s/%s",parentName,baseName);
	return registryGetValue(name,returned,maxSize);
}



/**
 * @ingroup registry
 */
int registrySetValueByParent(char *parentName, char *baseName, char *value) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	return registrySetValue(name,value);
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
 * live bellow system/sw/myApp, you'll use this method to get them all.
 *
 * Option can be any of the following, ORed:
 * - \c RG_O_RECURSIVE \n
 *   Retrieve also the keys under the child keys, recursively.
 *   The rg(1) ls command, with switch -R uses this option.
 * - \c RG_O_DIR \n
 *   By default, folder keys will not be returned because they don't have 
 *   values and exist only to define hierarchy. Use this option if you need 
 *   them to be included in the returned KeySet. 
 * - \c RG_O_NOVALUE \n
 *   Do not include in returned the regular value keys. The resulting KeySet
 *   will be only the skeleton of the tree. 
 * - \c RG_O_STATONLY \n
 *   Only stat(2) the keys; do not retrieve the value, comment and key data
 *   type. The resulting keys will be empty and usefull only for 
 *   informational purposes. The rg(1) ls command, without the -v switch 
 *   uses this option. 
 * - \c RG_O_INACTIVE \n
 *   Will make it not ignore inactive keys. So returned will be filled also 
 *   with inactive keys. See registry(7) to understand how inactive keys work.
 * - \c RG_O_SORT \n
 *   Will sort keys alphabetically by their names. 
 *
 * @param parentName name of the parent key
 * @param returned the KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @ingroup registry
 */
int registryGetChildKeys(char *parentName, KeySet *returned, unsigned long options) {
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
	registryGetFilename(&parentKey,buffer,sizeof(buffer));
	parentDir=opendir(buffer);
	if (!parentDir) return -1; /* Key is not a directory or doesn't exist. Propagate errno */

	realParentName=realloc(realParentName,parentNameSize=keyGetFullNameSize(&parentKey));
	keyGetFullName(&parentKey,realParentName,parentNameSize);

	while ((entry=readdir(parentDir))) {
		Key *keyEntry;
		char *transformedName=0;
		size_t keyNameSize=0;
		
		/* Ignore '.' and '..' directory entries */
		if (!strcmp(entry->d_name,".") || !strcmp(entry->d_name,"..")) continue;
		
// // // /*		/* Dots ('.') in key name must be escaped */
// // // 		while (cursor=index(cursor,'.')) {
// // // 			if (!transformedName) transformedName=realloc(transformedName,200);
// // // 			strncat(transformedName,lastCursor,cursor-lastCursor);
// // // 			strncat(transformedName,"\\",1);
// // // 			lastCursor=cursor;
// // // 			cursor++;
// // // 		} 
// // // 		/* copy the last chunk */
// // // 		if (transformedName) {
// // // 			strcat(transformedName,lastCursor);
// // // 			keyNameSize=strblen(transformedName);
// // // 		}*/
		
		/* If key name starts with '.', and don't want INACTIVE keys, ignore it */
		if ((*entry->d_name == '.') && !(options & RG_O_INACTIVE)) continue;

		/* Next 2 ifs are required to transform from UTF-8 */
		if (!transformedName) {
			transformedName=realloc(transformedName,keyNameSize=strblen(entry->d_name));
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
		
		if (options & RG_O_STATONLY) registryStatKey(keyEntry);
		else if (options & RG_O_NFOLLOWLINK) {
			registryStatKey(keyEntry);
			if (!keyIsLink(keyEntry)) registryGetKey(keyEntry);
		} else {
			int rc=registryGetKey(keyEntry);
			/* If this is a permission problem, at least stat the key */
			if (rc && errno==RG_KEY_RET_NOCRED) registryStatKey(keyEntry);
		}
		
		if (S_ISDIR(keyEntry->access)) {
			if (options & RG_O_RECURSIVE) {
				KeySet children;
				char *fullName;
				size_t fullNameSize;

				fullName=malloc(fullNameSize=keyGetFullNameSize(keyEntry));
				keyGetFullName(keyEntry,fullName,fullNameSize);

				ksInit(&children);
				/* Act recursively, without sorting. Sort in the end, once */
				registryGetChildKeys(fullName,&children, ~(RG_O_SORT) & options);
				
				/* Insert the current directory key in the returned list before its children */
				if (options & RG_O_DIR) ksAppend(returned,keyEntry);
				else {
					keyClose(keyEntry);
					free(keyEntry);
				}
				
				/* Insert the children */
				ksAppendKeys(returned,&children);
				free(fullName);
			} else if (options & RG_O_DIR) ksAppend(returned,keyEntry);
				else {
					keyClose(keyEntry);
					free(keyEntry);
				} 
		} else if (options & RG_O_NOVALUE) {
			keyClose(keyEntry);
			free(keyEntry);
		} else ksAppend(returned,keyEntry);
	} /* while(readdir) */
	keyClose(&parentKey);
	free(realParentName);

	if ((options & (RG_O_SORT)) && (returned->size > 1)) {
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
 * @ingroup registry
 */
int registryGetRootKeys(KeySet *returned) {
	Key *system=0,*user=0;
	
	system=malloc(sizeof(Key));
	keyInit(system);
	keySetName(system,"system");
	if (registryGetKey(system)) {
		free(system);
		system=0;
	}
	
	user=malloc(sizeof(Key));
	keyInit(user);
	keySetName(user,"user");
	if (registryGetKey(user)) {
		free(user);
		user=0;
	}
	
	if (system) ksInsert(returned,system);
	if (user) ksAppend(returned,user);
	
	return 0;
}



/**
 * @ingroup registry
 */
int registryStatKey(Key *key) {
	char keyFileName[800];
	struct stat keyFileNameInfo;
	size_t pos;
	u_int32_t semiflag;

	pos=registryGetFilename(key,keyFileName,sizeof(keyFileName));
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
	semiflag=RG_KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;
	key->flags |= RG_KEY_FLAG_ACTIVE;

	return 0;
}




/**
 * @ingroup registry
 */
int registryGetKey(Key *key) {
	char keyFileName[500];
	struct stat keyFileNameInfo;
	int fd;
	size_t pos;
	u_int32_t semiflag;

	pos=registryGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* something is wrong */

	if ((fd=open(keyFileName,O_RDONLY))==-1) return -1;
	fstat(fd,&keyFileNameInfo);
	keyFromStat(key,&keyFileNameInfo);
	if (!keyIsDir(key)) {
		FILE *input;

		input=fdopen(fd,"r");
		if (keyFileUnserialize(key,input)) {
			fclose(input);
			return -1;
		}
		fclose(input);
	} else close(fd);

	/* Remove the NEEDSYNC flag */
	semiflag=RG_KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;
	
	return 0;
}



/**
 * @ingroup registry
 */
int registrySetKeys(KeySet *ks) {
	Key *current;
	
	for (current=ks->start; current; current=current->next) {
		if (keyNeedsSync(current)) {
			registrySetKey(current);
		}
	}
	
	return 0;
}



/**
 * @ingroup registry
 */
int registrySetKey(Key *key) {
	char keyFileName[800];
	char folderMaker[800];
	char *cursor, *last;
	int fd;
	FILE *output=0;
	size_t pos;
	u_int32_t semiflag;
	struct stat stated;

	pos=registryGetFilename(key,keyFileName,sizeof(keyFileName));
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
					errno=RG_KEY_RET_INVALIDKEY;
					return -1;
				}
				for (cursor=index(cursor,'/');
						cursor && (cursor <= last);
						cursor=index(cursor,'/')) {
					strncpy(folderMaker,keyFileName,cursor-keyFileName);
					folderMaker[cursor-keyFileName]=0;
					/* TODO: use correct user's umask() for dir permissions */
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
			- other: It is an absolute FS path, or relative inside-registry
			  namespace path, and symlink it
		*/
	
		keyInit(&target);
		if (key->data) targetName=malloc(key->dataSize);
		keyGetLink(key,targetName,key->dataSize);
		
		/* Setting the name will let us know if this is a valid keyname */
		if (keySetName(&target,targetName)) {
			/* target has a valid key name */
			targetName=realloc(targetName,800);
			registryGetFilename(&target,targetName,800);
			keyClose(&target);
		} else if (errno==RG_KEY_RET_INVALIDKEY) {
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
		if (fd==-1) return -1;
		if (getuid() == 0) fchown(fd,keyGetUID(key),keyGetGID(key));
		if (!(output=fdopen(fd,"w+"))) return -1;
		if (keyFileSerialize(key,output)) {
			fclose(output);
			return -1;
		}
		fclose(output);
	}

	/* Remove the NEEDSYNC flag */
	semiflag=RG_KEY_FLAG_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;
	
	return 0;
}






/**
 * @ingroup registry
 */
int registryRemove(char *keyName) {
	Key key;
	char fileName[800];
	off_t rc;

	keyInit(&key);
	rc=keySetName(&key,keyName);
	if (rc==-1) return -1;
	rc=registryGetFilename(&key,fileName,sizeof(fileName));
	keyClose(&key);
	if (!rc) return -1;

	return remove(fileName);
}





/**
 * @ingroup registry
 */
int registryLink(char *oldPath,char *newKeyName) {
	Key key;
	int rc;
	
	keyInit(&key);
	keySetName(&key,newKeyName);
	keySetLink(&key,oldPath);
	
	rc=registrySetKey(&key);
	keyClose(&key);
	
	return rc;
}

