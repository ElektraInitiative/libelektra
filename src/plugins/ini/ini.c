/***************************************************************************
            ini.c  -  Backend for ini-style like files
                             -------------------
    begin                : 01.03.2005
    updated              : 06.10.2005
    copyright            : (C) 2005 by Markus Raab
    email                : debian@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#include <ini.h>


int kdbOpen_ini(KDB *handle)
{
	KDBCap *cap = kdbhGetCapability (handle);

	cap->onlyFullGet=1;
	cap->noStat=1;

	cap->onlyRemoveAll=1;

	cap->onlyFullSet=1;
	cap->onlyAddKeys=1;

	cap->onlySystem=1;
	cap->onlyUser=1;

	cap->noOwner=1;
	cap->noValue=1;
	cap->noComment=1;
	cap->noUID=1;
	cap->noGID=1;
	cap->noMode=1;
	cap->noDir=1;
	cap->noATime=1;
	cap->noMTime=1;
	cap->noCTime=1;
	cap->noRemove=1;
	cap->noMount=1;
	cap->noBinary=1;
	cap->noString=1;
	cap->noTypes=1;
	cap->noError=1;

	cap->noLock=1;
	cap->noThread=1;

	kdbhSetBackendData (handle, malloc (sizeof (backendData)));

	/* backend initialization logic */

	return 0;
}

int kdbClose_ini(KDB *handle)
{
	free (kdbhGetBackendData (handle));

	/* free all backend resources and shut it down */

	return 0; /* success */
}

ssize_t kdbGet_ini(KDB *handle, KeySet *returned, const Key *parentKey)
{
	int rc = 0;
	char t [MAX_PATH_LENGTH];
	Key * write = keyDup (parentKey);
	
#if DEBUG
	file_name(write, t);
	fprintf (stderr, "file_name: %s\n",	t);
	base_name(write, t);
	fprintf (stderr, "base_name: %s\n",	t);
#endif

	/**Immediately call IniChooseFile (will work recursively)*/
	rc = IniChooseFile (handle, write, returned, 0);
	keyDel (write);

	return rc;
}

ssize_t kdbSet_ini(KDB *handle, KeySet *ks, const Key *parentKey)
{
	return IniSetKeys (handle, ks);
}


/**
 * Returns a filename from the Key which can be opened for forKey
 *
 * The name returned should be correct, because
 * it removes subdirs and the keyname.
 *
 * It even tests with kdbStatKey if the File exists.
 * So be prepaired that stat information is filled
 * within the key.
 *
 * @see IniSearchFileName
 * @param filename must be allocated MAX_PATH_LENGTH space.
 * Previous Content of Filename will be destroyed afterwards.
 * 
 * @ingroup ini
 */
size_t IniSearchFileName (KDB *handle, Key * forKey, char * filename)
{
	size_t length;
	uint8_t info = 0;
	char * end;

	length = file_name (forKey, filename);
	
	do {
#if DEBUG
		fprintf (stderr, "Search %s\n", filename);
#endif
		end = strrchr (filename, '/');
		if (end == NULL) {
#if DEBUG
			fprintf (stderr, "Could not find any file\n");
#endif
			return -1;
		}
		*end= '\0';
		stat_file (forKey, filename);
		info = keyGetType (forKey);
	} while (!(info & KEY_TYPE_FILE));

	return length;
}


/**
 * Get out all the keys of a file
 * 
 * @param keyFileName: Name of the file
 * @param keyRoot: Name of the root of files
 *  The root will be added before the keyName
 * 
 * @ingroup ini
 */
ssize_t IniGetKeys (KDB *handle, char * keyFileName, char * keyRoot, KeySet * returned)
{
	Key * key;
	int pos;

	if (open_file (handle, keyFileName, O_RDONLY) == -1)
	{
#if DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		/*errno = KDB_ERR_NOTFOUND;*/
		return -1;
	}
	
	key = keyNew(0);

#if DEBUG
	fprintf (stderr, "Call read_key(%s)\n", keyRoot);
#endif
	while ((pos=read_key (handle, key, keyRoot)) == 0)
	{
#if DEBUG
		fprintf (stderr, "Append key\n");
#endif
		ksAppendKey(returned,key);

		key = keyNew(0);
	}
	
	keyDel (key); /* delete the not used key left*/

	close_file(handle);
	
	return 0; /* success */
}

/**
 * Reads the contents of the whole file which Key
 * points to.
 *
 * The Keys will be added in the Keyset.
 *
 * @ingroup ini
 */
int IniReadFile (KDB *handle, Key * key, KeySet * returned, unsigned long options)
{
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keyLength;

#if DEBUG
	fprintf (stderr, "IniReadfile\n");
#endif
	
	file_name(key, filename);

	keyLength = keyGetNameSize (key);
	keyname = malloc (keyLength);
	keyGetName (key, keyname, keyGetNameSize (key));

#if DEBUG
	fprintf (stderr, "Call IniGetKeys(filename: %s, keyRoot: %s,returned)\n", 
		filename, keyname);
#endif
	IniGetKeys (handle, filename, keyname, returned);
	
	if (keyLength >0) free (keyname);

	return 0;
}

/**
 * This mapper chooses between the different
 * styles of files to start the correct function.
 *
 * For files it starts IniReadFile
 * For directorys it starts IniReadDir
 *
 * @ingroup ini
 * */
int IniChooseFile(KDB *handle, Key * key, KeySet * returned, unsigned long options)
{
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keylength;
	
	file_name(key, filename);
	stat_file (key, filename);

#if DEBUG
	fprintf (stderr, "IniChooseFile, pathName: %s\n", filename);
#endif
	
	if (keyGetType (key) == KEY_TYPE_DIR)
	{
		if (filename[strlen(filename)-1] != '/')
		{
			keylength = keyGetNameSize(key);
			keyname = malloc (keylength + 2);
			keyGetName (key, keyname, keylength);
			keyname[keylength-1] = '/';
			keyname[keylength] = '\0';
			keySetName (key, keyname);
			free (keyname);
		}
			
		return IniReadDir (handle, key, returned, options);
	}

	if (keyGetType (key) == KEY_TYPE_FILE)
	{
		return IniReadFile (handle, key, returned, options);
	}

#if DEBUG
	fprintf (stderr, "Not a directory or file!");
#endif
	return -1;
}


/**
 * Reads all Keys of a directory.
 *
 * For every key the mapper IniChooseFile
 * will be called.
 * 
 * @ingroup ini
 * */
int IniReadDir(KDB *handle, Key * key, KeySet * returned, unsigned long options)
{
	char pathName [MAX_PATH_LENGTH];
	char keyname [MAX_PATH_LENGTH];
	char keypath [MAX_PATH_LENGTH];
	char filename [MAX_PATH_LENGTH];
	void * dir;
	int ret;

#if DEBUG
	fprintf (stderr, "IniReadDir\n");
#endif
	file_name (key, pathName);
	dir = open_dir (pathName);
	if (dir == NULL) {
		fprintf (stderr, "Could not open directory %s\n", pathName);
		return -1;
	}
	
	keyGetName (key, keypath, MAX_PATH_LENGTH);
	
	while (read_dir (dir,filename) == 0)
	{
		if (	strcmp(filename, ".")  == 0 || 
			strcmp(filename, "..") == 0)
			continue;
		
		if (filename[0] == '.' && !(options & KDB_O_INACTIVE))
			continue;

#if DEBUG
		fprintf (stderr, "Next entry filename: %s\n", filename);
#endif
		strncpy(keyname, keypath, MAX_PATH_LENGTH);
		strcat(keyname, filename);
		keySetName (key, keyname);

#if DEBUG
		fprintf (stderr, "New keyname: %s\n", keyname);
#endif
		ret = IniChooseFile (handle, key, returned, options);
	}

	if (close_dir (dir))
	{
		fprintf (stderr, "Could not close directory\n");
		return -1;
	}

	return ret;
}



/**Walks through a file and lookups if the first key of a keyset is in the
 * given Keyset. When found it overwrites the key. It also checks if any
 * of the other keys can be written in that file under that context. If
 * not, the keys will remain in the Keyset.
 *
 * So, the KeySet will be empty after complete successful writing. At
 * least one key is guranteed to be written when it was successful.
 * 
 * The whole idea behind that concept is, that open/close and searching
 * within a file is complete minimized. The disadvantage is, that a
 * fast ksLookupKey is required, because every readed key will be compared
 * with all keys in keyset (its not implemented in elektra right now,
 * so it will consume linear more time, but only in memory, not on
 * harddisc).
 *
 * When not found it writes out the keys before the sectionend.
 * This can be:
 * 
 * setting keys new keys will introduce new folders and files
 * as needed.
 *
 * At least one key will be written!
 * 
 * @return >=0 on success
 * @return -1 on failure (Keyset may be changed then!)
 * @return #nr when #nr keys could not written to file
 *
 * */
int IniSetKeys (KDB *handle, KeySet * origKeys)
{
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName = NULL;
	char * keyRoot = NULL;
	char * end;

	Key * origKey;	/*First key which will introduce opening file*/
	Key * setKey;	/*Used for getting the keys which may be set*/
	Key * key = keyNew(0);

	long oldpos;
	
#if DEBUG
	fprintf (stderr, "IniSetKeys() entered\n");
#endif

	ksRewind (origKeys);
	origKey = ksNext (origKeys); /* Open file for this key*/
	key = keyDup (origKey);	/* for searching*/
	
	pos = IniSearchFileName(handle, key, keyFileName);

#if DEBUG
	fprintf (stderr, "after SearchFileName ...\n");
#endif

	if (pos == -1) /* no such file exists*/
	{
		file_name(key,keyFileName);
		create_dir(keyFileName);	
	}
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	if (keyFullName == NULL) goto memerror;
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	/* dirname*/
	if (end) *end = 0;
	keyRoot = malloc (strlen (keyFullName));
	if (keyRoot == NULL) goto memerror;
	strcpy (keyRoot, keyFullName);
	if (end) *end = '/';	/*revert keyname*/

#if DEBUG
	fprintf (stderr, "keyRoot: %s, keyName: %s\n", keyRoot, keyFullName);
	fprintf (stderr, "Set Key [%d] in File: %s\n",keySize, keyFileName);
#endif

	if (open_file (handle, keyFileName, O_RDWR) == -1)
	{
#if DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		ksAppendKey(origKeys, origKey);
		/*errno = KDB_ERR_NOTFOUND;*/
		goto fileerror;
	}
	
	while ((pos=read_key (handle, key, keyRoot)) == 0)
	{
		if ((setKey = ksLookupByName (origKeys, key->key, 0)) != NULL) 
		{	/* right Key found*/
#if DEBUG
			fprintf (stderr, "Key found\n");
			fprintf(stderr, "Name: (%s), Value: (%s), Comment: (%s)\n",
				keyName (setKey), (char *) keyValue(setKey),
				(char *) keyComment (setKey));
#endif
			
			write_key(handle, setKey, oldpos);

			if ((keyCompare (key, origKey) & KEY_NAME) == 0)
				pos = 1; /*Start Key found, good!*/
		}
		oldpos = ftell (FILEPTR);
	}
	if (pos != 1) {	/* key not found, add to the end*/
#if DEBUG
		fprintf (stderr, "Key not found!\n");
#endif
		fseek (FILEPTR, 0, SEEK_END);
		oldpos = ftell(FILEPTR);
		/*TODO: write key here if not found
		 * write_key(setKey, oldpos);*/
		pos = 0;
	} else if (pos == 1) { /* key found, everything went ok!*/
		pos = 0;
	}
	
	close_file (handle);
	
	free (keyFullName);
	free (keyRoot);
	
	keyDel (key);

#if DEBUG
	fprintf (stderr, "leaving IniSetKeys()\n");
#endif
	
	return pos; /* success */

memerror:
	/*errno=KDB_ERR_NOMEM;*/
	close_file (handle);
#if DEBUG
	fprintf (stderr, "Memory Error\n");
#endif
fileerror:
	
	free (keyFullName);
	free (keyRoot);
	
	keyDel (key);
	
	return -1;
}

ELEKTRA_PLUGIN_EXPORT(ini)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_ini,
		KDB_BE_CLOSE,	&kdbClose_ini,
		KDB_BE_GET,	&kdbGet_ini,
		KDB_BE_SET,	&kdbSet_ini,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Markus Raab <elektra@libelektra.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION, "Key/Value Pairs are stored in files in following scheme: key1=value1;comment",
		KDB_BE_END);
}

