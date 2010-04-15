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



#include "filesys.h"

int kdbOpen_filesys(KDB *handle)
{
	KDBCap * cap = kdbhGetCapability(handle);

	cap->noMode=1;
	cap->noMount=1;
	cap->noError=1;

	kdbhSetCapability (handle,cap);
#if DEBUG && VERBOSE
	fprintf (stderr, "you open filesys " BACKENDVERSION "\n");
#endif

	return 0;
}




int kdbClose_filesys(KDB *handle)
{
	return 0;
}


ssize_t kdbGet_filesys(KDB *handle, KeySet *returned, const Key *parentKey)
{
	DIR *parentDir;
	char buffer[MAX_KEY_LENGTH];
	struct dirent *entry;
	Key *keyEntry;
	Key *found;
	KeySet *appended = ksNew (ksGetSize(returned)*2, KS_END);
	int errnosave = errno;

	/*
		- Convert parent key name into a real filename
		- Check if it is a directory. Open it
		- Browse, read and include in the KeySet
	*/
	if (kdbbGetFullFilename(handle,parentKey,buffer,sizeof(buffer)) == -1)
	{
		errno = errnosave;
		return -1;
	}
	parentDir=opendir(buffer);

	/* Check if Key is not a directory or doesn't exist.
	 * Propagate errno */
	if (!parentDir) {
		errno=KDB_ERR_NODIR;
		ksDel (appended);
		errno = errnosave;
		return -1;
	}

	/* add the parentKey itself
	Return value of kdbGetKey_filesys is irrelevant, because we already know its a directory */
	keyEntry=keyDup(parentKey);
	kdbGetKey_filesys(handle,keyEntry);
	ksAppendKey(appended,keyEntry);

	while ((entry=readdir(parentDir))) {

		/* Ignore '.', '..' and file containing information about directory itself */
		if (!strcmp(entry->d_name,".") ||
				!strcmp(entry->d_name,"..") ||
				!strcmp(entry->d_name,DIR_FILENAME))
			continue;

		keyEntry = keyNew(0);
		if (kdbbGetFullKeyName (handle, entry->d_name, parentKey, keyEntry) == -1)
		{
			closedir (parentDir);
			errno = errnosave;
			return -1;
		}

		found = ksLookup (returned, keyEntry, KDB_O_WITHOWNER|KDB_O_POP);
		if (found)
		{
			keyDel (keyEntry);
			keyEntry = found; /* use found instead */
		}
		if (keyNeedStat(parentKey)) set_bit (keyEntry->flags, KEY_FLAG_STAT);
		if (kdbGetKey_filesys(handle,keyEntry) != -1)
		{
			keyflag_t semiflag;
			/* Remove the SYNC flag */
			semiflag= KEY_FLAG_SYNC;
			semiflag=~semiflag;
			keyEntry->flags &= semiflag;
			ksAppendKey(appended,keyEntry);
		}
		else keyDel (keyEntry);
	} /* while(readdir) */

	closedir(parentDir);

	ksClear (returned); /* delete all other keys */
	ksAppend (returned, appended);
	ksDel (appended);

	errno = errnosave;
	return returned->size;
}



/**
 * A probably inefficient implementation for kdbSet()
 * method, but in case of filesys it is also the most effective one, because every
 * key is independed.
 *
 * @see kdbSet(), kdbSet_backend()
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 */
ssize_t kdbSet_filesys(KDB *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	Key *current=ksCurrent(returned);

	if (!current) current=ksNext(returned);
	while (current) {
		if (keyNeedRemove(current))
		{
			if (kdbRemoveKey_filesys (handle, current))
			{
				errno = errnosave;
				return -1;
			}
		}
		else if (keyNeedSync(current))
		{
			if (kdbSetKey_filesys(handle,current))
			{
				errno = errnosave;
				return -1;
			}
		}
		current=ksNext(returned);
	}

	errno = errnosave;
	return 0;
}


/*************************************
 *           Helpers                 *
 *************************************/



int kdbGetKey_filesys(KDB *handle, Key *key) {
	char keyFilename[MAX_KEY_LENGTH];
	struct stat keyFilenameInfo;
	int fd;
	ssize_t pos;
	keyflag_t semiflag;
	FILE *input;


	pos=kdbbGetFullFilename(handle,key,keyFilename,sizeof(keyFilename));
	if (pos == -1) return -1; /* something is wrong */

	stat(keyFilename,&keyFilenameInfo);
	keyFromStat(key,&keyFilenameInfo);

	if (keyNeedStat (key))
	{
		/* Remove the SYNC flag */
		semiflag= KEY_FLAG_SYNC;
		semiflag=~semiflag;
		key->flags &= semiflag;
		return 0;
	}

	if ((fd=open(keyFilename,O_RDONLY))==-1)
	{
		if (errno==ENOTDIR) errno=KDB_ERR_NOTFOUND;
		return -1;
	}

	if (keyIsDir(key)) {
		close(fd);
		strcat(keyFilename,"/");
		strcat(keyFilename,DIR_FILENAME);
		if ((fd=open(keyFilename,O_RDONLY))!=-1)
		{
			input=fdopen(fd,"r");
			kdbbReadLock (input);
			if (keyFileUnserialize(key,input))
			{
				kdbbUnlock (input);
				fclose(input);
				return -1;
			}
			kdbbUnlock (input);
			fclose(input);
		}
	} else {
		input=fdopen(fd,"r");
		kdbbReadLock (input);
		if (keyFileUnserialize(key,input)) {
			kdbbUnlock (input);
			fclose(input);
			return -1;
		}
		kdbbUnlock (input);
		fclose(input);
	}

	/* Remove the SYNC flag */
	semiflag=KEY_FLAG_SYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return 0;
}


int keyToFile(KDB *handle, Key *key, char *keyFilename) {
	int fd=0;
	FILE *output=0;
	int errnosave;

	/* Open key file with its full file name */
	fd=open(keyFilename,O_CREAT | O_RDWR | O_TRUNC, key->mode);
	if (fd==-1) return -1;
	if (!(output=fdopen(fd,"w+"))) return -1;

	kdbbWriteLock (output);

	/* Set permissions, might fail without problems */
	errnosave = errno;
	fchown(fd,keyGetUID(key),keyGetGID(key));
	fchmod(fd,key->mode);
	errno = errnosave;


	/* Write file content */
	if (keyFileSerialize(key,output)) {
		kdbbUnlock (output);
		fclose(output);
		return -1;
	}

	kdbbUnlock (output);
	fclose(output);

	return 0;
}

int kdbSetKey_filesys(KDB *handle, Key *key) {
	char keyFilename[MAX_KEY_LENGTH];
	char genericBuffer[MAX_PATH_LENGTH];
	char *cursor=0, *last=0;
	ssize_t pos=0;
	keyflag_t semiflag=0;
	struct stat stated;
	int rc=0;
	int exists=0;
	int errnosave;

	pos=kdbbGetFullFilename(handle,key,keyFilename,sizeof(keyFilename));
	if (pos == -1) return -1; /* Something is wrong. Propagate errno. */

	exists = ! stat(keyFilename,&stated);

	if (! exists && errno!=ENOENT && errno!=ENOTDIR)
		return -1; /* propagate errno */

	if (! exists) {
		/* Entry does not exist in the filesystem. */
		/* It is our job to create it. */
		/* Now this block will take care to check or create
		   entire file path to key */

		/* check if parent dir already exists */
		last=strrchr(keyFilename,(int)'/');
		strncpy(genericBuffer,keyFilename,last-keyFilename);
		genericBuffer[last-keyFilename]=0;

		/* first test existence of immediate parent */
		if (stat(genericBuffer,&stated) || !S_ISDIR(stated.st_mode)) {
			/* create all path recursively until before our basename */
			mode_t parentMode = 0777; /* KEY_DEF_MODE | KEY_DEF_DIR; */

			last   =  strrchr(keyFilename,'/');
			cursor =   strchr(keyFilename,'/'); cursor++; /* skip first occurence */
			if (!last || !cursor) { /* bizarre key name */
				errno=KDB_ERR_INVALIDKEY;
				return -1;
			}

			/* The deep dir maker loop */
			for (cursor=strchr(cursor,'/');
							cursor && (cursor <= last);
							cursor=strchr(cursor,'/')) {

				strncpy(genericBuffer,keyFilename,cursor-keyFilename);
				genericBuffer[cursor-keyFilename]=0;

#ifdef HAVE_WIN32
				if (mkdir(genericBuffer)<0) {
#else
				if (mkdir(genericBuffer,parentMode)<0) {
#endif
					if (errno==EEXIST) {
						/* There is something there already. Check. */
						stat(genericBuffer,&stated);

						if (!S_ISDIR(stated.st_mode)) {
							/* Trying to create a dir on something that is not. */
							/* Convert into a dir in a very low level way. */
							char tempName[MAX_PATH_LENGTH];
							char finalName[MAX_PATH_LENGTH];

							sprintf(tempName,"%s.%d",genericBuffer,rand());

							if (rename(genericBuffer,tempName)<0) return -1;

							/* Now tries to create the dir again */
#ifdef HAVE_WIN32
							if (mkdir(genericBuffer)<0) {
#else
							if (mkdir(genericBuffer,parentMode)<0) {
#endif
								/* Rollback on failure */
								rename(tempName,genericBuffer);
								return -1;
							}

							sprintf(finalName,"%s/%s",genericBuffer,DIR_FILENAME);

							if (rename(tempName,finalName)<0) return -1;
						} /* Regular key converted into a dir key. */
					} else return -1; /* propagate errno */
				}

#ifdef HAVE_WIN32
				/* Since mkdir on win32 can't set a mode for us we need to do it manually */
				if(chmod(genericBuffer, parentMode) < 0)
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
			rc=unlink(keyFilename);
			if (rc && errno!=ENOENT) return -1;
		}

#ifdef HAVE_WIN32
		if (mkdir(keyFilename)<0 && errno!=EEXIST)
			return -1; /* propagate errno */
		/* Since mkdir on win32 can't set a mode for us we need to do it manually */
		if (chmod(keyFilename, key->mode)<0) return -1;
#else
		if (mkdir(keyFilename,key->mode)<0 && errno!=EEXIST)
			return -1;
#endif

		/* Dir permissions... */
		errnosave = errno;
		chown(keyFilename,keyGetUID(key),keyGetGID(key));
		chmod(keyFilename,key->mode);
		errno = errnosave;

		/* Save Value, Comment and Type... */
		strcat(keyFilename,"/"); /* Append a filename for key data and comment */
		strcat(keyFilename,DIR_FILENAME);
		rc=keyToFile(handle,key,keyFilename);
	} else { /* key is not dir */
		if ( exists && S_ISDIR(stated.st_mode) ) {
			/* but inode there is a dir */
			DIR *dir=0;
			int hasChild=0;
			struct dirent *entry=0;

			/* Check if it has child keys */
			dir=opendir(keyFilename);
			if (!dir) return -1;
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
				errno=KDB_ERR_NOTEMPTY;
				return -1;
			}

			/* We'll have to transform it to a non-dir key, so... */
			/* Remove the directory file if any */
			sprintf(genericBuffer,"%s/%s",keyFilename,DIR_FILENAME);
			rc=unlink(genericBuffer);
			if (rc && errno!=ENOENT) return -1;

			/* Remove the dir */
			rc=rmdir(keyFilename);
			if (rc) return -1;
		}

		/* Its a plain key, so simply write to disk */
		rc=keyToFile(handle,key,keyFilename);
	} /* END OF: key is not dir */

	if (rc == 0) {
		/* Remove the SYNC flag */
		semiflag=KEY_FLAG_SYNC;
		semiflag=~semiflag;
		key->flags &= semiflag;
	}

	return rc;
}


int kdbRemoveKey_filesys(KDB *handle, Key *key) {
	char fileName[MAX_PATH_LENGTH];
	ssize_t rc;
	struct stat stated;
	keyflag_t semiflag;

	rc=kdbbGetFullFilename(handle,key,fileName,sizeof(fileName));
	if (rc == -1) return -1;

	if (stat(fileName,&stated)) return -1;

	if ( S_ISDIR(stated.st_mode) ) {
		/* inode there is a dir */
		DIR *dir=0;
		int hasChild=0;
		int hasDataEntry=0;
		char dataFilename[MAX_PATH_LENGTH];
		struct dirent *entry=0;

		/* Check if it has child keys */
		dir=opendir(fileName);
		if (!dir) return -1;
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
			sprintf(dataFilename,"%s/%s",fileName,DIR_FILENAME);
			rc=remove(dataFilename);
			if (rc && errno!=ENOENT) return -1;
		}
	}

	/* Remove the SYNC flag */
	semiflag=KEY_FLAG_SYNC;
	semiflag=~semiflag;
	key->flags &= semiflag;

	return remove(fileName);
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
	unsigned int nversion=0;
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
		errno=KDB_ERR_INVALIDKEY;
		return -1;
	}

	nversion=atoi(version+2);
	if (!nversion || nversion != RG_KEY_FORMAT_VERSION) {
		errno=KDB_ERR_INVALIDKEY;
		return -1;
	}

	if (!fgets(type,    sizeof(type),    input)) return -1;

	while (readComment) {
		if (fgets(generalBuffer,sizeof(generalBuffer),input)) {
			if (memcmp(generalBuffer,"<DATA>\n\0",8)) {
				/* This is not the beginning of the data part so it is part of comment */
				currentBufferSize=kdbiStrLen(generalBuffer);
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

	if (comment && kdbbUTF8Engine(UTF8_FROM,&comment,&commentSize)) {
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

	if (keyIsString (key))
	{
		if (kdbbUTF8Engine(UTF8_FROM,&data,&dataSize)) {
			free(data);
			return -1;
		}
		keySetRaw(key,data,dataSize);
	} else if (keyIsBinary(key))
	{
		/* Binary decoded data. */
		char *decoded=0;
		size_t decodedSize;

		/* raw data is maximum half the size of text-decoded data */
		decodedSize=dataSize/2;

		decoded=malloc(decodedSize);
		if (!(decodedSize=kdbbDecode(data,decoded))) return -1;
		keySetRaw(key,decoded,decodedSize);
		free(decoded);
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

	fprintf(output,"RG%03d\n",RG_KEY_FORMAT_VERSION);
	fprintf(output,"%d\n", keyGetType (key));
	if (key->comment) {
		if (kdbbNeedsUTF8Conversion()) {
			size_t convertedCommentSize=key->commentSize;
			char *convertedComment=malloc(convertedCommentSize);

			memcpy(convertedComment,key->comment,key->commentSize);
			if (kdbbUTF8Engine(UTF8_TO,&convertedComment,&convertedCommentSize)) {
				free(convertedComment);
				return -1;
			}
			fprintf(output,"%s\n",convertedComment);
			free(convertedComment);
		} else fprintf(output,"%s\n",key->comment);
	}

	fputs("<DATA>\n",output);
	fflush(output);

	if (key->dataSize) {
		/* There is some data to write */
		if (keyIsString (key)) {
			/* String or similar type of value */
			if (kdbbNeedsUTF8Conversion()) {
				size_t convertedDataSize=key->dataSize;
				char *convertedData=malloc(convertedDataSize);

				memcpy(convertedData,key->data,key->dataSize);
				if (kdbbUTF8Engine(UTF8_TO,&convertedData,&convertedDataSize)) {
					free(convertedData);
					return -1;
				}
				fprintf(output,"%s",convertedData);
				free(convertedData);
			} else fputs(key->data,output);
		} else if (keyIsBinary (key)) {
			/* Binary values */
			char *encoded=malloc(3*key->dataSize+1);
			size_t encodedSize;

			encodedSize=kdbbEncode(key->data,key->dataSize,encoded);
			fwrite(encoded,encodedSize,1,output);
			free(encoded);
		} // else unhandeled: has data but neighter string nor binary, so drop the data
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
	keySetUID(key,stat->st_uid);
	keySetGID(key,stat->st_gid);

	// remove directory bit
	keySetMode(key,stat->st_mode & 0777);

	keySetATime(key, stat->st_atime);
	keySetMTime(key, stat->st_mtime);
	keySetCTime(key, stat->st_ctime);
	return 0;
}



KDBEXPORT(filesys)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_filesys,
		KDB_BE_CLOSE,	&kdbClose_filesys,
		KDB_BE_GET,	&kdbGet_filesys,
		KDB_BE_SET,	&kdbSet_filesys,
		KDB_BE_VERSION,	BACKENDVERSION,
		KDB_BE_AUTHOR,	"Avi Alkalay <avi@unix.sh>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"Every key is represented by a single file",
		KDB_BE_END);
}
