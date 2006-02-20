/***************************************************************************
            filesys.c  -  Methods for accessing the Key Database
                             -------------------
    begin                : Mon Dec 25 2004
    copyright            : (C) 2004 by Avi Alkalay
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

*/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <kdb.h>
#include <kdbbackend.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <assert.h>

#define BACKENDNAME "filesys"

/**Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posixsystem*/
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif


/* These are some helpers we'll define bellow */
int keyFileUnserialize(Key *key,FILE *input);
size_t kdbGetFilename(const Key *forKey,char *returned,size_t maxSize);
int keyFileSerialize(Key *key, FILE *output);
int keyFromStat(Key *key,struct stat *stat);




int kdbOpen_filesys() {
	/* backend initialization logic */
	return 0;
}




int kdbClose_filesys() {
	/* free all backend resources and shutdown */
	return 0;
}



int kdbStatKey_filesys(Key *key) {
	char keyFileName[MAX_PATH_LENGTH];
	struct stat keyFileNameInfo;
	size_t pos;
	uint32_t semiflag;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* something is wrong */

	if (lstat(keyFileName,&keyFileNameInfo)) return -1;
	keyFromStat(key,&keyFileNameInfo);

	if (keyIsLink(key) && key->recordSize) {
		key->data=malloc(key->recordSize+1); /* Add 1 byte for ending 0 */

		key->dataSize=readlink(keyFileName,key->data,key->recordSize);
		((char *)key->data)[key->recordSize]=0; /* null terminate it */
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_SWITCH_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;
	key->flags |= KEY_SWITCH_ACTIVE; /* obsolete.... */

	return 0;
}



int kdbGetKey_filesys(Key *key) {
	char keyFileName[500];
	struct stat keyFileNameInfo;
	int fd;
	size_t pos;
	uint32_t semiflag;
	FILE *input;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* something is wrong */

	if ((fd=open(keyFileName,O_RDONLY))==-1) return -1;
	/* TODO: lock at this point */
	fstat(fd,&keyFileNameInfo);
	keyFromStat(key,&keyFileNameInfo);
	if (!keyIsDir(key)) {
		input=fdopen(fd,"r");
		if (keyFileUnserialize(key,input)) {
			fclose(input);
			return -1;
		}
		/* TODO: unlock at this point */
		fclose(input);
	} else close(fd);

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_SWITCH_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}



int kdbSetKey_filesys(Key *key) {
	char keyFileName[MAX_PATH_LENGTH];
	char folderMaker[MAX_PATH_LENGTH];
	char *cursor, *last;
	int fd;
	FILE *output=0;
	size_t pos;
	uint32_t semiflag;
	struct stat stated;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* Something is wrong. Propagate errno. */

	if (stat(keyFileName,&stated))
		if (errno==ENOENT) {
			/* check if parent dir already exists */
			last=strrchr(keyFileName,(int)'/');
			strncpy(folderMaker,keyFileName,last-keyFileName);
			folderMaker[last-keyFileName]=0;
			if (stat(folderMaker,&stated)) {
				/* create all path recursively until before our basename */
				mode_t parentMode;
				mode_t umaskValue=umask(0);
				
				umask(umaskValue);
				#if defined(S_IRWXU) && defined(S_IRWXG) && defined(S_IRWXO)
				parentMode=((S_IRWXU | S_IRWXG | S_IRWXO) & (~ umaskValue)) |
					S_IWUSR | S_IXUSR;  /* from coreutils::mkdir.c */
				#else
				parentMode=(~ umaskValue) |
					S_IWUSR | S_IXUSR;  /* from coreutils::mkdir.c */
				#endif
				
				last   =strrchr(keyFileName,'/');
				cursor = strchr(keyFileName,'/'); cursor++; /* skip first occurence */
				if (!last || !cursor) { /* bizarre key name */
					errno=KDB_RET_INVALIDKEY;
					return -1;
				}
				for (cursor=strchr(cursor,'/');
						cursor && (cursor <= last);
						cursor=strchr(cursor,'/')) {
					strncpy(folderMaker,keyFileName,cursor-keyFileName);
					folderMaker[cursor-keyFileName]=0;
					#ifdef HAVE_WIN32
					if (mkdir(folderMaker)<0 && errno!=EEXIST)
					#else
					if (mkdir(folderMaker,parentMode)<0 && errno!=EEXIST)
					#endif
						return -1;       /* propagate errno */
					/* Since mkdir on win32 can't set a mode for us we need to do it manually */
					#ifdef HAVE_WIN32
					if(chmod(folderMaker, parentMode) < 0)
						return -1;
					#endif
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

	/* Enough of checking. Real write now, with a bit of other checks :-) */

	if (keyIsLink(key)) {
		char targetName[MAX_PATH_LENGTH];
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

		/* Setting the name will let us know if this is a valid keyname */
		if (keySetName(&target,key->data)) {
			/* target has a valid key name */
			kdbGetFilename(&target,targetName,sizeof(targetName));
			keyClose(&target);
		} else if (errno==KDB_RET_INVALIDKEY) {
			/* Is an invalid key name. So treat it as a regular file */
			strncpy(targetName,key->data,sizeof(targetName));
			keyClose(&target); /* get rid of invalid stuff */
		} else {
			keyClose(&target); /* get rid of invalid stuff */
			return -1; /* propagate errno from keySetName() */
		}


		/* Now, targetName has the real destination of our link */

		/* TODO: handle null targetName */
		rc=symlink(targetName,keyFileName);

		return rc; /* propagate errno */
	} else if (keyIsDir(key)) {
		#ifdef HAVE_WIN32
		if (mkdir(keyFileName)<0 && errno!=EEXIST)
		#else
		if (mkdir(keyFileName,key->access)<0 && errno!=EEXIST)
		#endif
			return -1;       /* propagate errno */
		/* Since mkdir on win32 can't set a mode for us we need to do it manually */
		#ifdef HAVE_WIN32
		if(chmod(keyFileName, key->access) < 0)
			return -1;
		#endif
	} else {
		/* Try to open key file with its full file name */
		/* TODO: Make it more "transactional" without truncating */
		fd=open(keyFileName,O_CREAT | O_RDWR | O_TRUNC, key->access);
		if (fd==-1) return -1;
		/* TODO: lock file here */
		if (getuid() == 0) fchown(fd,key->uid,key->gid);
		if (getuid() == key->uid || getgid() == key->gid) fchmod(fd,key->access);
		if (!(output=fdopen(fd,"w+"))) return -1;
		if (keyFileSerialize(key,output)) {
			fclose(output);
			return -1;
		}
		/* TODO: unlock file here */
		fclose(output);
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_SWITCH_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}



int kdbRename_filesys(Key *key, const char *newName) {
	char oldFileName[MAX_PATH_LENGTH];
	char newFileName[MAX_PATH_LENGTH];
	Key *newKey;
	int rc;
	
	newKey=keyNew(0);
	rc=keySetName(newKey,newName);
	if (rc == 0) {
		keyDel(newKey);
		return -1;
	}
	
	rc=kdbGetFilename(key,oldFileName,sizeof(oldFileName));
	if (rc == 0) {
		keyDel(newKey);
		return -1;
	}
	
	rc=kdbGetFilename(newKey,newFileName,sizeof(newFileName));
	keyDel(newKey); /* won't need it anymore */
	if (rc == 0) return -1;
	
	return rename(oldFileName,newFileName);
}




int kdbRemoveKey_filesys(const Key *key) {
	char fileName[MAX_PATH_LENGTH];
	off_t rc;

	rc=kdbGetFilename(key,fileName,sizeof(fileName));
	if (!rc) return -1;

	return remove(fileName);
}




ssize_t kdbGetKeyChildKeys_filesys(const Key *parentKey, KeySet *returned, unsigned long options) {
	size_t parentNameSize=keyGetFullNameSize(parentKey);
	char *realParentName=NULL;
	DIR *parentDir;
	char buffer[MAX_PATH_LENGTH];
	struct dirent *entry;

	realParentName = (char *)malloc(sizeof(char) *parentNameSize);

	/*
		- Convert parent key name into a real filename
		- Check if it is a directory. Open it
		- Browse, read and include in the KeySet
	*/
	kdbGetFilename(parentKey,buffer,sizeof(buffer));
	parentDir=opendir(buffer);

	/* Check if Key is not a directory or doesn't exist.
	 * Propagate errno */
	if (!parentDir) {
		free(realParentName);
		return -1;
	}
	keyGetFullName(parentKey,realParentName,parentNameSize);

	while ((entry=readdir(parentDir))) {
		Key *keyEntry;
		char *transformedName=0;
		size_t keyNameSize=0;

		/* Ignore '.' and '..' directory entries */
		if (!strcmp(entry->d_name,".") || !strcmp(entry->d_name,".."))
			continue;

		/* If key name starts with '.', and don't want INACTIVE keys, ignore it */
		if ((*entry->d_name == '.') && !(options & KDB_O_INACTIVE))
			continue;

		/* Next 2 ifs are required to transform filename from UTF-8 */
		if (!transformedName) {
			transformedName=
				realloc(transformedName,keyNameSize=strblen(entry->d_name));
			assert(transformedName!=NULL);
			strcpy(transformedName,entry->d_name);
		}
		if (UTF8Engine(UTF8_FROM,&transformedName,&keyNameSize)) {
			free(transformedName);
			free(realParentName);
			closedir(parentDir);
			return -1;  /* propagate errno */
		}

		/* Copy the entire transformed key name to our final buffer */
		sprintf(buffer,"%s/%s",realParentName,transformedName);
		free(transformedName); transformedName=0; /* don't need it anymore */

		keyEntry=keyNew(buffer,KEY_SWITCH_END);


		/* TODO: inefficient code in next block */
		if (options & KDB_O_STATONLY) kdbStatKey_filesys(keyEntry);
		else if (options & KDB_O_NFOLLOWLINK) {
			kdbStatKey_filesys(keyEntry);
			if (!keyIsLink(keyEntry)) kdbGetKey_filesys(keyEntry);
		} else {
			int rc=kdbGetKey_filesys(keyEntry);
			/* If this is a permission problem, at least stat the key */
			if (rc && errno==KDB_RET_NOCRED) kdbStatKey_filesys(keyEntry);
		}


		if (keyIsDir(keyEntry)) {
			if (options & KDB_O_RECURSIVE) {
				KeySet *children;

				children=ksNew();
				/* Act recursively, without sorting. Sort in the end, once */
				kdbGetKeyChildKeys_filesys(keyEntry,children,
					~(KDB_O_SORT) & options);

				/* Insert the current directory key in the returned list
				 * before its children */
				if (options & KDB_O_DIR) ksAppend(returned,keyEntry);
				else keyDel(keyEntry);

				/* Insert the children */
				ksAppendKeys(returned,children);
				ksDel(children);
			} else if (options & KDB_O_DIR) ksAppend(returned,keyEntry);
				else keyDel(keyEntry);
		} else if (options & KDB_O_DIRONLY) keyDel(keyEntry);
			else ksAppend(returned,keyEntry);
	} /* while(readdir) */
	
	closedir(parentDir);

	free(realParentName);

	if ((options & (KDB_O_SORT)) && (returned->size > 1))
		ksSort(returned);

	return returned->size;
}













/**
 *
 * <b>internal usage only-</b>
 * @ingroup internals
 *
 */
int handleOldKeyFileVersion(Key *key,FILE *input,uint16_t nversion) {
	char generalBuffer[BUFFER_SIZE];
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

							--commentSize; /* remove awareness of previous \0 */
							buffer=malloc(commentSize+currentBufferSize);
							strcpy(buffer,comment);
							strcat(buffer,generalBuffer);
							comment=realloc(comment,commentSize+=currentBufferSize);
							assert(comment != NULL);
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
						assert(data!=NULL);
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
				uint8_t oldVersion=atoi(type);
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

			if (key->type <= KEY_TYPE_BINARY) {
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
 * @param key the pre-initialized key that will contain our data.
 * @param input the opened file from which we want to read.
 * @return 0 on success.
 * @ingroup internals
 */
int keyFileUnserialize(Key *key,FILE *input) {
	char generalBuffer[BUFFER_SIZE];
	size_t currentBufferSize;

	char version[10];
	uint16_t nversion=0;
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

					--commentSize; /* remove awareness of previous \0 */
					buffer=malloc(commentSize+currentBufferSize);
					strcpy(buffer,comment);
					strcat(buffer,generalBuffer);
					comment=realloc(comment,commentSize+=currentBufferSize);
					assert(comment!=NULL);
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
				assert(data!=NULL);
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

	/* TODO: test this.... */
	if (key->type >= KEY_TYPE_STRING) {
		if (UTF8Engine(UTF8_FROM,&data,&dataSize)) {
			free(data);
			return -1;
		}
		keySetRaw(key,data,dataSize);
	} else {
		/* Binary data. Unencode. */
		char *unencoded=0;
		size_t unencodedSize;

		/* raw data is maximum half the size of text-encoded data */
		unencodedSize=dataSize/2;

		unencoded=malloc(unencodedSize);
		if (!(unencodedSize=unencode(data,unencoded))) return -1;
		keySetRaw(key,unencoded,unencodedSize);
		free(unencoded);
	}

	free(data);

	return 0;
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

	dataSize=key->dataSize;
	if (dataSize) {
		/* There is some data to write */
		if (key->type >= KEY_TYPE_STRING) {
			/* String or similar type of value */
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
			/* Binary values */
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
size_t keyCalcRelativeFileName(const Key *key,char *relativeFileName,size_t maxSize) {
/*	 if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	} */
/*
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
*/
	if (kdbNeedsUTF8Conversion()) {
		char *converted;
		size_t size;

		if (!(size=keyGetNameSize(key))) return 0;

		converted=malloc(size);
		keyGetName(key,converted,size);

/* 		memcpy(converted,relativeFileName,convertedSize); */

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
/*	if (!key) {
		errno=KDB_RET_NULLKEY;
		return -1;
}*/

	keySetAccess(key,stat->st_mode);
	keySetUID(key,stat->st_uid);
	keySetGID(key,stat->st_gid);
	
	if (S_ISDIR(stat->st_mode)) keySetType(key,KEY_TYPE_DIR);
	else keySetType(key,key->type & (~KEY_TYPE_DIR)); /* remove the DIR flag */
	
	if (S_ISLNK(stat->st_mode)) keySetType(key,KEY_TYPE_LINK);
	else keySetType(key,key->type & (~KEY_TYPE_LINK)); /* remove the LINK flag */
	
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
size_t kdbGetFilename(const Key *forKey,char *returned,size_t maxSize) {
	size_t length=0;

	switch (keyGetNamespace(forKey)) {
		case KEY_NS_SYSTEM: {
			/* Prepare to use the 'system/ *' database */
			strncpy(returned,KDB_DB_SYSTEM,maxSize);
			length=strlen(returned);
			break;
		}
		/* If we lack a usable concept of users we simply let the default handle it 
		 * and hence disable the entire user/ hiarchy. */
		#ifdef HAVE_PWD_H
		case KEY_NS_USER: {
			/* Prepare to use the 'user:????/ *' database */
			struct passwd *user=0;

			if (forKey->userDomain) user=getpwnam(forKey->userDomain);
			else user=getpwnam(getenv("USER"));
			
			if (!user) return 0; /* propagate errno */
			length=snprintf(returned,maxSize,"%s/%s",user->pw_dir,KDB_DB_USER);
			break;
		  /* Need to find userdir in a different way */
			return 0;
		}
		#endif

		default: {
			errno=KDB_RET_INVALIDKEY;
			return 0;
		}
	}

	returned[length]='/'; length++;
	length+=keyCalcRelativeFileName(forKey,returned+length,maxSize-length);

	return length;
}



KDBEXPORT(filesys)
{	
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,         &kdbOpen_filesys,
		KDB_BE_CLOSE,        &kdbClose_filesys,
		KDB_BE_GETKEY,       &kdbGetKey_filesys,
		KDB_BE_SETKEY,       &kdbSetKey_filesys,
		KDB_BE_STATKEY,      &kdbStatKey_filesys,
		KDB_BE_RENAME,       &kdbRename_filesys,
		KDB_BE_REMOVEKEY,    &kdbRemoveKey_filesys,
		KDB_BE_GETCHILD,     &kdbGetKeyChildKeys_filesys,
		
		/* Explicitly set to default methods: 
		 * We shouldn't explicitly set defaults. This is handled
		 *  inside the core of elektra much better */
/*		KDB_BE_SETKEYS,      &kdbSetKeys_default,
		KDB_BE_MONITORKEY,   &kdbMonitorKey_default,
		KDB_BE_MONITORKEYS,  &kdbMonitorKeys_default,*/
		KDB_BE_END);
}
