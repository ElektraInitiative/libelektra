/***************************************************************************
            filesys.c  -  A filesystem backend implementation for Elektra
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
 *   Elektra. Each Key is a file in the filesystem.                        *
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

#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <kdbbackend.h>

#define BACKENDNAME "filesys"
#define PATH_SEPARATOR '/'
#define ESCAPE_CHAR '\\'

/**Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posixsystem*/
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else
#define MAX_PATH_LENGTH 4096
#endif


char *DIR_FILENAME="%%dirdata";


/* These are some helpers we'll define bellow */
int keyFileUnserialize(Key *key,FILE *input);
size_t kdbGetFilename(const Key *forKey,char *returned,size_t maxSize);
int keyFileSerialize(Key *key, FILE *output);
int keyFromStat(Key *key,struct stat *stat);
int relativeFileNameToKeyName(const char *string, char *buffer, int bufSize);




int kdbOpen_filesys(KDBHandle *handle) {
	/* backend initialization logic */
	return 0;
}




int kdbClose_filesys(KDBHandle *handle) {
	/* free all backend resources and shutdown */
	return 0;
}



int kdbStatKey_filesys(KDBHandle handle, Key *key) {
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

	if (keyIsDir(key)) {
		/* check if there is a directory data file */
		strcat(keyFileName,"/");
		strcat(keyFileName,DIR_FILENAME);

		if (lstat(keyFileName,&keyFileNameInfo)) {
			keyFromStat(key,&keyFileNameInfo);
			keySetDir(key,keyFileNameInfo.st_mode);
		}
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_SWITCH_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}



int kdbGetKey_filesys(KDBHandle handle, Key *key) {
	char keyFileName[MAX_PATH_LENGTH];
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

	if (keyIsDir(key)) {
		close(fd);
		strcat(keyFileName,"/");
		strcat(keyFileName,DIR_FILENAME);
		if ((fd=open(keyFileName,O_RDONLY))!=-1) {
			/* there is a directory data file there */
			/* TODO: lock at this point */
			fstat(fd,&keyFileNameInfo);
			keyFromStat(key,&keyFileNameInfo);
			keySetDir(key,keyFileNameInfo.st_mode);

			input=fdopen(fd,"r");
			if (keyFileUnserialize(key,input)) {
				fclose(input);
				return -1;
			}
			/* TODO: unlock at this point */
			fclose(input);
		}
	} else {
		input=fdopen(fd,"r");
		if (keyFileUnserialize(key,input)) {
			fclose(input);
			return -1;
		}
		/* TODO: unlock at this point */
		fclose(input);
	}

	/* Remove the NEEDSYNC flag */
	semiflag=KEY_SWITCH_NEEDSYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}


int keyToFile(KDBHandle handle, Key *key, char *keyFileName) {
	int fd=0;
	FILE *output=0;

	/* Try to open key file with its full file name */
	/* TODO: Make it more "transactional" without truncating */
	fd=open(keyFileName,O_CREAT | O_RDWR | O_TRUNC, key->access);
	if (fd==-1) return -1;
	/* TODO: lock file here */

	/* Set permissions */
	if (kdbhGetUID(handle) == 0) fchown(fd,key->uid,key->gid);
	if (kdbhGetGID(handle) == key->uid || kdbhGetGID(handle) == key->gid)
		fchmod(fd,key->access);


	/* Write file content */
	if (!(output=fdopen(fd,"w+"))) return -1;
	if (keyFileSerialize(key,output)) {
		fclose(output);
		return -1;
	}
	/* TODO: unlock file here */
	fclose(output);

	return 0;
}


int kdbSetKey_filesys(KDBHandle handle, Key *key) {
	char keyFileName[MAX_PATH_LENGTH];
	char genericBuffer[MAX_PATH_LENGTH];
	char *cursor=0, *last=0;
	size_t pos=0;
	uint32_t semiflag=0;
	struct stat stated;
	int rc=0;
	int exists=0;

	pos=kdbGetFilename(key,keyFileName,sizeof(keyFileName));
	if (!pos) return -1; /* Something is wrong. Propagate errno. */

	exists = ! stat(keyFileName,&stated);

	if (! exists && errno!=ENOENT) return -1; /* propagate errno */

	if (! exists) {
		/* Entry does not exist in the filesystem. */
		/* It is our job to create it. */
		/* Now this block will take care to check or create
		   entire file path to key */

		/* check if parent dir already exists */
		last=strrchr(keyFileName,(int)'/');
		strncpy(genericBuffer,keyFileName,last-keyFileName);
		genericBuffer[last-keyFileName]=0;
		if (stat(genericBuffer,&stated)) {
			/* create all path recursively until before our basename */
			mode_t parentMode;
			mode_t umaskValue=kdbhGetUMask(handle);

#if defined(S_IRWXU) && defined(S_IRWXG) && defined(S_IRWXO)
			parentMode=((S_IRWXU | S_IRWXG | S_IRWXO) & (~ umaskValue)) |
				S_IWUSR | S_IXUSR;  /* from coreutils::mkdir.c */
#else
			/* TODO: check this piece of code */
			parentMode=(~ umaskValue) |
				S_IWUSR | S_IXUSR;  /* from coreutils::mkdir.c */
#endif

			last   =  strrchr(keyFileName,'/');
			cursor =   strchr(keyFileName,'/'); cursor++; /* skip first occurence */
			if (!last || !cursor) { /* bizarre key name */
				errno=KDB_RET_INVALIDKEY;
				return -1;
			}

			/* The deep dir maker loop */
			for (cursor=strchr(cursor,'/');
							 cursor && (cursor <= last);
							 cursor=strchr(cursor,'/')) {

				strncpy(genericBuffer,keyFileName,cursor-keyFileName);
				genericBuffer[cursor-keyFileName]=0;

#ifdef HAVE_WIN32
				if (mkdir(genericBuffer)<0 && errno!=EEXIST)
					return -1; /* propagate errno */
				/* Since mkdir on win32 can't set a mode for us we need to do it manually */
				if(chmod(genericBuffer, parentMode) < 0)
					return -1;
#else
				if (mkdir(genericBuffer,parentMode)<0 && errno!=EEXIST)
					return -1;
#endif
				cursor++;
			} /* END OF: dir maker loop */
		} /* END OF: parent is not there */
	} /* END OF: !exists (or, preparation for key creation) */


	if (keyIsDir(key)) {
		if ( exists && !S_ISDIR(stated.st_mode) ) {
			/* New key is dir, but file there is not */
			/* Remove the file */
			rc=unlink(keyFileName);
			if (rc && errno!=ENOENT) return -1;
		}

#ifdef HAVE_WIN32
		if (mkdir(keyFileName)<0 && errno!=EEXIST)
			return -1; /* propagate errno */
		/* Since mkdir on win32 can't set a mode for us we need to do it manually */
		if (chmod(keyFileName, key->access)<0) return -1;
#else
		if (mkdir(keyFileName,key->access)<0 && errno!=EEXIST)
			return -1;
#endif

		/* Dir permissions... */
		if (getuid() == 0 || kdbhGetUID(handle) == 0)
			chown(keyFileName,key->uid,key->gid);
		if (getuid() == 0 ||
					(kdbhGetUID(handle) == key->uid ||
					kdbhGetGID(handle) == key->gid))
			chmod(keyFileName,key->access);

		/* Value and Comment... */
		if (key->data || key->comment) {
			/* Append a filename for key data and comment */
			strcat(keyFileName,"/");
			strcat(keyFileName,DIR_FILENAME);
			rc=keyToFile(handle,key,keyFileName);
		}
	} else { /* key is not dir */
		if ( exists && S_ISDIR(stated.st_mode) ) {
			/* but inode there is a dir */
			DIR *dir=0;
			int hasChild=0;
			struct dirent *entry=0;

			/* Check if it has child keys */
			dir=opendir(keyFileName);
			while ( !hasChild && (entry=readdir(dir)) ) {
				/* Ignore '.' and '..' directory entries */
				if (!strcmp(entry->d_name,".") ||
								 !strcmp(entry->d_name,"..") ||
								 !strcmp(entry->d_name,DIR_FILENAME))
					continue;
				else hasChild=1;
			}
			closedir(dir);

			if (hasChild) {
				/* Dir contains files, so can't un-dir it */
				errno=ENOTEMPTY;
				return -1;
			}

			/* We'll have to transform it to a non-dir key, so... */
			/* Remove the directory file if any */
			sprintf(genericBuffer,"%s/%s",keyFileName,DIR_FILENAME);
			rc=unlink(genericBuffer);
			if (rc && errno!=ENOENT) return -1;

			/* Remove the dir */
			rc=rmdir(keyFileName);
			if (rc) return -1;
		}

		if (keyIsLink(key)) {
			char targetName[MAX_PATH_LENGTH];
			Key target;
			int rc;

				/*
			If targetName starts with:
			- "system" | "user" | any future root name: Convert to a FS path,
			and symlink it.
			- other: It is an absolute FS path, or relative inside-kdb
			namespace path, and symlink it.
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
		} /* END OF: key is link */

		else
			/* Its a plain key, so simply write to disk */
			rc=keyToFile(handle,key,keyFileName);
	} /* END OF: key is not dir */

	if (rc == 0) {
		/* Remove the NEEDSYNC flag */
		semiflag=KEY_SWITCH_NEEDSYNC;
		semiflag=~semiflag;
		key->flags &= semiflag;
	}

	return rc;
}



int kdbRename_filesys(KDBHandle handle, Key *key, const char *newName) {
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

	newKey->userDomain=key->userDomain;

	rc=kdbGetFilename(key,oldFileName,sizeof(oldFileName));
	if (rc == 0) {
		/* undo hack */
		newKey->userDomain=0;
		keyDel(newKey);
		return -1;
	}

	rc=kdbGetFilename(newKey,newFileName,sizeof(newFileName));
	/* undo hack */
	newKey->userDomain=0;
	keyDel(newKey); /* won't need it anymore */
	if (rc == 0) return -1;

	return rename(oldFileName,newFileName);
}




int kdbRemoveKey_filesys(KDBHandle handle, const Key *key) {
	char fileName[MAX_PATH_LENGTH];
	off_t rc;
	struct stat stated;

	rc=kdbGetFilename(key,fileName,sizeof(fileName));
	if (!rc) return -1;

	if (stat(fileName,&stated)) return -1;

	if ( S_ISDIR(stated.st_mode) ) {
		/* inode there is a dir */
		DIR *dir=0;
		int hasChild=0;
		int hasDataEntry=0;
		char dataFileName[MAX_PATH_LENGTH];
		struct dirent *entry=0;

		/* Check if it has child keys */
		dir=opendir(fileName);
		while ( !hasChild && (entry=readdir(dir)) ) {
			/* Ignore DIR_FILENAME, '.' and '..' directory entries */
			if (!strcmp(entry->d_name,".") ||
							!strcmp(entry->d_name,"..") ||
							(hasDataEntry=!strcmp(entry->d_name,DIR_FILENAME)))
				continue;
			else hasChild=1;
		}
		closedir(dir);

		if (hasChild) {
			/* Dir contains files, so can't un-dir it */
			errno=ENOTEMPTY;
			return -1;
		}

		if (hasDataEntry) {
			/* We'll have to transform it to a non-dir key, so... */
			/* Remove the directory file if any */
			sprintf(dataFileName,"%s/%s",fileName,DIR_FILENAME);
			rc=remove(dataFileName);
			if (rc && errno!=ENOENT) return -1;
		}
	}

	return remove(fileName);
}




ssize_t kdbGetKeyChildKeys_filesys(KDBHandle handle, const Key *parentKey,
		KeySet *returned, unsigned long options) {
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
		char *keyName;
		size_t keyNameSize=0;

		/* Ignore '.' and '..' directory entries */
		if (!strcmp(entry->d_name,".") ||
				!strcmp(entry->d_name,"..") ||
				/* Ignore also the dir data file.
					This is the job of the caller, and not ours. */
				!strcmp(entry->d_name,DIR_FILENAME))
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

		/* Translate from filename -> keyname */
		keyName = (char *) malloc(keyNameSize*3);
		relativeFileNameToKeyName(transformedName, keyName, keyNameSize*3);

		/* Copy the entire transformed key name to our final buffer */
		sprintf(buffer,"%s/%s",realParentName,keyName);
		free(transformedName); transformedName=0; /* don't need it anymore */

		keyEntry=keyNew(buffer,KEY_SWITCH_END);
		/* Copy key domain from parent key */
		keySetOwner(keyEntry,parentKey->userDomain);
		/* keySetOwner(keyEntry,kdbhGetUserName(handle)); */

		/* TODO: inefficient code in next block */
		if (options & KDB_O_STATONLY) kdbStatKey_filesys(handle,keyEntry);
		else if (options & KDB_O_NFOLLOWLINK) {
			kdbStatKey_filesys(handle,keyEntry);
			if (!keyIsLink(keyEntry)) kdbGetKey_filesys(handle,keyEntry);
		} else {
			int rc=kdbGetKey_filesys(handle,keyEntry);
			/* If this is a permission problem, at least stat the key */
			if (rc && errno==KDB_RET_NOCRED)
				kdbStatKey_filesys(handle,keyEntry);
		}


		if (keyIsDir(keyEntry)) {
			if (options & KDB_O_RECURSIVE) {
				KeySet *children;

				children=ksNew();
				/* Act recursively, without sorting. Sort in the end, once */
				kdbGetKeyChildKeys_filesys(handle,keyEntry,children,
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
 * This is the counterpart of keyFileUnserialize().
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
			char *encoded=malloc(3*dataSize+1);
			size_t encodedSize;

			encodedSize=encode(key->data,dataSize,encoded);
			fwrite(encoded,encodedSize,1,output);
			free(encoded);
		}
	}
	return 0;
}

/**
 * Char encoding
 *
 * Encode '/', '\', '%', '+', ' ' char following
 * RFC 2396 or copy char untouched if different.
 *
 * @param c Char to encode
 * @param buffer string wich will contain encoded char
 * @param bufSize Size of the buffer
 * @return: Size of the encoded string if success or -1
 * if error  * (then buffer is untouched)
 * @ingroup internals
 * @see decodeChar
 *
 * NOTE: No '\0' is added at the end of buffer.
 *
 */
int encodeChar(char c, char *buffer, size_t bufSize) {
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
 * Char decoding
 *
 * Decode one char from %25, %2B, %2F, %2C following
 * RFC 2396 or copy char untouched if different.
 *
 * @param from String containing sequence to decode
 * @param into Decoded char
 * @return: Positive size of byte read from "from" for decoding
 * the sequence if sucess or -1 if error (into untouched)
 * @ingroup internals
 * @see encodeChar
 *
 * NOTE: No '\0' is added at the end of buffer.
 *
 */
int decodeChar(const char *from, char *into)
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
 * @return 0 on success, -1 on failure (
 * buffer is always '\0' terminated)
 * @ingroup internals
 * @see keyNameToRelativeFileName
 *
 */
int relativeFileNameToKeyName(const char *string, char *buffer, int bufSize)
{
	char decoded;
	int j;

	while ( *(string) != '\0' && bufSize > sizeof(char) ) {

		if ( *string == PATH_SEPARATOR ) {
			/* Translate PATH_SEPARATOR into KEY_DELIM */
			*(buffer++) = RG_KEY_DELIM;
			bufSize -= sizeof(char);
			string++;
		} else {
			/* Decode char */
			if ( (j = decodeChar(string, &decoded)) != -1 ) {
				string += j;
				*(buffer++) = decoded;
				bufSize -= sizeof(char);
			} else {
				*(buffer) = '\0';
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
 * @param buffer encoded filename
 * @param bufSize Size of buffer
 * @return Number of byte written in buffer on success,
 * -1 on failure (buffer is always '\0' terminated)
 * @ingroup internals
 * @see keyNameToRelativeFileName
 *
 **/
int keyNameToRelativeFileName(const char *string, char *buffer, size_t bufSize)
{
	size_t	written;
	int     j;

	written = 0;
	while ( (*string != '\0') && bufSize > sizeof(char) ) {

		if ( *string == ESCAPE_CHAR && *(string+1) == RG_KEY_DELIM ) {
			/* Key delimiter escaped, encode these two (escape + delim) */
			if ( (j = encodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				buffer += j;
				written += j*sizeof(char);
			} else {
				return -1;
			}

			if ( (j = encodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				written += j*sizeof(char);
				buffer += j;
			} else {
				return -1;
			}

		} else if ( *string == RG_KEY_DELIM ) {
			/* Replace unescaped KEY_DELIM to PATH_SEPARATOR */
			*(buffer++) = PATH_SEPARATOR;
			bufSize -= sizeof(char);
			written += sizeof(char);
			string++;

		} else {
			/* Encode ... */
			if ( (j = encodeChar(*(string++), buffer, bufSize)) != -1 ) {
				bufSize -= j*sizeof(char);
				written += j*sizeof(char);
				buffer += j;
			} else {
				return -1;
			}
		}
	}
	*buffer = '\0';
	written++;

	return written;
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

	if (kdbNeedsUTF8Conversion()) {
		char *converted;
		size_t size;

		if (!(size=keyGetNameSize(key))) return 0;

		converted = (char *) malloc(MAX_PATH_LENGTH);
		size = keyNameToRelativeFileName(keyStealName(key), converted,
			MAX_PATH_LENGTH);

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
	} else {
		return keyNameToRelativeFileName(keyStealName(key), relativeFileName, maxSize);
	}

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
	keySetAccess(key,stat->st_mode);
	keySetUID(key,stat->st_uid);
	keySetGID(key,stat->st_gid);

	if (! keyIsDir(key))
		/* TODO: review */
		keySetType(key,key->type & (~KEY_TYPE_DIR)); /* remove the DIR flag */

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

			if (forKey->userDomain)
				user=getpwnam(forKey->userDomain);
			else if ( getenv("USER") )
				user=getpwnam(getenv("USER"));

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
