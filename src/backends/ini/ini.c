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

/**
 * Initialize the backend.
 *
 * It does not do anything.
 * Apps anyway must call it, to make sure
 * to be compatibel to other backends.
 * 
 * @return 0 on success
 * @ingroup ini
 */
int kdbOpen_ini(KDBHandle *handle) {
	return 0;
}




/**
 * Closes the backend.
 * 
 * It does not do anything.
 * Apps anyway must call it, to make sure
 * to be compatibel to other backends.
 * 
 * @return 0 on success
 * @ingroup ini
 */
int kdbClose_ini(KDBHandle *handle) {
	return 0; /* success */
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
size_t IniSearchFileName (Key * forKey, char * filename)
{
	size_t length;
	uint8_t info = 0;
	char * end;

	length = file_name (forKey, filename);
	
	do {
#ifdef DEBUG
		fprintf (stderr, "Search %s\n", filename);
#endif
		end = strrchr (filename, '/');
		if (end == NULL) {
#ifdef DEBUG
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
 * Implementation for kdbGetKey() method.
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup ini
 */
int kdbGetKey_ini(KDBHandle handle, Key *key) {
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName;
	char * keyRoot;
	char * end;
	
#ifdef DEBUG	
	fprintf (stderr, "kdbGetKey_ini() entered\n");
#endif
	
	pos = IniSearchFileName(key, keyFileName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	/* dirname*/
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	strcpy (keyRoot, keyFullName);
	*end = '/';
	
#ifdef DEBUG
	fprintf (stderr, "keyRoot: %s\n", keyRoot);
#endif
	
	if (! pos) {
		fprintf (stderr, "Could not receive filename");
		return -1;
	}

#ifdef DEBUG
	fprintf (stderr, "Get Key [%d] in File: %s\n", keySize, keyFileName);
#endif

	if (open_file (keyFileName, O_RDONLY) == -1)
	{
#ifdef DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		errno = KDB_RET_NOTFOUND;
		return -1;
	}
	
	while ((pos=read_key (key, keyRoot)) == 0)
	{
#ifdef DEBUG
		fprintf (stderr, "Compare: %s with %s\n", key->key, keyFullName);
#endif
		if (strcmp (key->key, keyFullName) == 0) {	/* right Key found*/
#ifdef DEBUG
			fprintf (stderr, "Key found\n");
#endif
			/*Useless setting of keyname again
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);*/

#ifdef DEBUG
			fprintf (stderr, "<KEY>%s<DATA>%s<COMMENT>%s\n", 
				key->key, (char *) key->data, key->comment);
#endif
		
			
			pos = 1;
			break;
		}
	}
	if (pos != 1) {	/* key not found, leave it, so that app won't sigfault*/
		errno = KDB_RET_NOKEY;
		pos = -1;
#ifdef DEBUG
		fprintf (stderr, "Key not found!\n");
#endif
	} else if (pos == 1) { /* key found, everything went ok!*/
		pos = 0;
	}
	
	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	return pos; /* success */
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
ssize_t kdbGetKeys (KDBHandle handle, char * keyFileName, char * keyRoot, KeySet * returned)
{
	Key * key;
	int pos;

	if (open_file (keyFileName, O_RDONLY) == -1)
	{
#ifdef DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		errno = KDB_RET_NOTFOUND;
		return -1;
	}
	
	key = keyNew (KEY_SWITCH_END);

#ifdef DEBUG
	fprintf (stderr, "Call read_key(%s)\n", keyRoot);
#endif
	while ((pos=read_key (key, keyRoot)) == 0)
	{
#ifdef DEBUG
		fprintf (stderr, "Append key\n");
#endif
		ksAppend (returned,key);

		key = keyNew (KEY_SWITCH_END);
	}
	
	keyDel (key); /* delete the not used key left*/

	close_file();
	
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
int IniReadFile (KDBHandle handle, Key * key, KeySet * returned, unsigned long options)
{
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keyLength;

#ifdef DEBUG
	fprintf (stderr, "IniReadfile\n");
#endif
	
	file_name(key, filename);

	keyLength = keyGetNameSize (key);
	keyname = malloc (keyLength);
	keyGetName (key, keyname, keyGetNameSize (key));

#ifdef DEBUG
	fprintf (stderr, "Call kdbGetKeys(filename: %s, keyRoot: %s,returned)\n", 
		filename, keyname);
#endif
	kdbGetKeys (handle, filename, keyname, returned);
	
	if (keyLength >0) free (keyname);

	return 0;
}

/**
 * This mapper chooses between the different
 * styles of files to start the correct function.
 *
 * For files it starts IniReadFile
 * For directorys it starts IniReadDir
 * TODO: Links, Subdirs
 *
 * @ingroup ini
 * */
int IniChooseFile(KDBHandle handle, Key * key, KeySet * returned, unsigned long options)
{
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keylength;
	
	file_name(key, filename);
	stat_file (key, filename);

#ifdef DEBUG
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

#ifdef DEBUG
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
int IniReadDir(KDBHandle handle, Key * key, KeySet * returned, unsigned long options)
{
	char pathName [MAX_PATH_LENGTH];
	char keyname [MAX_PATH_LENGTH];
	char keypath [MAX_PATH_LENGTH];
	char filename [MAX_PATH_LENGTH];
	void * dir;
	int ret;

#ifdef DEBUG
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

#ifdef DEBUG
		fprintf (stderr, "Next entry filename: %s\n", filename);
#endif
		strncpy(keyname, keypath, MAX_PATH_LENGTH);
		strcat(keyname, filename);
		keySetName (key, keyname);

#ifdef DEBUG
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



/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * 
 * @ingroup ini
 */
ssize_t kdbGetKeyChildKeys_ini(KDBHandle handle, const Key * key, KeySet *returned, unsigned long options)
{
	char t [MAX_PATH_LENGTH];
	Key * write;
	keyDup (key, write);
	
#ifdef DEBUG
	file_name(write, t);
	fprintf (stderr, "file_name: %s\n",	t);
	base_name(write, t);
	fprintf (stderr, "base_name: %s\n",	t);
	/**Immediately call IniChooseFile (will work recursively)*/
#endif

	return IniChooseFile (handle, write, returned, options);
}

/**Walks through a file and Lookups if the found key is in the
 * given Keyset. When found it overwrites the key.
 *
 * When not found it writes out the keys before the sectionend.
 * This can be:
 * 
 * setting keys new keys will introduce new folders and files
 * as needed.
 * 
 * @return 0 on success
 * @return -1 on failure
 * @return #nr when #nr keys could not written to file
 *
 * */
int IniSetKeys (KeySet * origKeys)
{
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName = NULL;
	char * keyRoot = NULL;
	char * end;

	Key * origKey;
	Key * setKey;	
	Key * key = keyNew(KEY_SWITCH_END);

	long oldpos;
	
#ifdef DEBUG
	fprintf (stderr, "IniSetKeys() entered\n");
#endif

	ksRewind (origKeys);
	origKey = ksCurrent (origKeys); /* Open file for this key*/
	keyDup (origKey, key);	/* for searching*/
	
	pos = IniSearchFileName(key, keyFileName);

#ifdef DEBUG
	fprintf (stderr, "after SearchFileName ...\n");
#endif

	if (pos == -1) /* no such file exists*/
	{
		file_name (key, keyFileName);
		create_dir(keyFileName);	
	}
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	if (keyFullName == NULL) goto memerror;
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	/* dirname*/
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	if (keyRoot == NULL) goto memerror;
	strcpy (keyRoot, keyFullName);
	*end = '/';	/*revert keyname*/

#ifdef DEBUG
	fprintf (stderr, "keyRoot: %s\n", keyRoot);
	fprintf (stderr, "Set Key [%d] in File: %s\n",keySize, keyFileName);
#endif

	if (open_file (keyFileName, O_RDWR) == -1)
	{
#ifdef DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		ksInsert (origKeys, origKey);
		errno = KDB_RET_NOTFOUND;
		goto fileerror;
	}
	
	while ((pos=read_key (key, keyRoot)) == 0)
	{
		if ((setKey = ksLookupByName (origKeys, key->key, 0)) != NULL) 
		{	/* right Key found*/
#ifdef DEBUG
			fprintf (stderr, "Key found\n");
			fprintf(stderr, "Name: (%s), Value: (%s), Comment: (%s)\n",
				keyStealName (setKey), (char *) keyStealValue(setKey),
				(char *) keyStealComment (setKey));
#endif
			
			write_key(setKey, oldpos);

			if ((keyCompare (key, origKey) & KEY_SWITCH_NAME) == 0)
				pos = 1; /*Start Key found, good!*/
		}
		oldpos = ftell (fc);
	}
	if (pos != 1) {	/* key not found, add to the end*/
#ifdef DEBUG
		fprintf (stderr, "Key not found!\n");
#endif
		fseek (fc, 0, SEEK_END);
		oldpos = ftell(fc);
		/*TODO: write key here if not found
		 * write_key(setKey, oldpos);*/
		pos = 0;
	} else if (pos == 1) { /* key found, everything went ok!*/
		pos = 0;
	}
	
	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	keyClose (key);
	
	return pos; /* success */

memerror:
	errno=KDB_RET_NOMEM;
	close_file ();
#ifdef DEBUG
	fprintf (stderr, "Memory Error\n");
#endif
fileerror:
	
	free (keyFullName);
	free (keyRoot);
	
	keyClose (key);
	
	return -1;
}

/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup ini
 */
int kdbSetKey_ini(KDBHandle handle, Key *origkey) {
	int rc;
	KeySet * ks = ksNew ();
	
	ksInsert (ks, origkey);
	
	rc = IniSetKeys (ks);

	ksDel (ks);
	return rc;
}





/**
 * Implementation for kdbSetKeys() method.
 * 
 * The implementation of this method is optional, and a builtin, probablly 
 * inefficient implementation can be explicitly used when exporting the
 * backend with kdbBackendExport(), using kdbSetKeys_default().
 * 
 * @see kdbSetKeys() for expected behavior.
 * @ingroup ini
 */
int kdbSetKeys_ini(KDBHandle handle, KeySet *ks) {
	int rc;
	do {
		rc = IniSetKeys (ks);
	} while (rc < 1);
	
	return rc;
}


#define SETKEY_SET 0
#define SETKEY_DELETE 1


/**Finds and remove a key.
 * 
 * Removeing keys may remove folders and files.
 *
 * Files will be removed when all keys are deleted and then it there is a
 * request to remove the DIR which refers to the file.
 *
 * Folders will be removed when all keys inside it are deleted and
 * there is a request to remove the DIR which refers the folder.
 * */
int IniRemoveKey (Key * origkey, int op)
{
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName = NULL;
	char * keyRoot = NULL;
	char * end;

	Key psetKey;
	Key pkey;
	Key * setKey = &psetKey;
	Key * key = &pkey;

	long oldpos;
	
#ifdef DEBUG
	fprintf (stderr, "IniRemoveKey() entered\n");
#endif

	keyInit (setKey);
	keyInit (key);
	keyDup (origkey, setKey); /* for writing*/
	keyDup (origkey, key);	/* for searching*/
	
	pos = IniSearchFileName(key, keyFileName);

#ifdef DEBUG
	fprintf (stderr, "after SearchFileName ...\n");
#endif

	if (pos == -1) /* no such file exists*/
	{
		file_name (key, keyFileName);
		create_dir(keyFileName);	
	}
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	if (keyFullName == NULL) goto memerror;
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	/* dirname*/
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	if (keyRoot == NULL) goto memerror;
	strcpy (keyRoot, keyFullName);
	*end = '/';

#ifdef DEBUG
	fprintf (stderr, "keyRoot: %s\n", keyRoot);
	fprintf (stderr, "Set Key [%d] in File: %s\n",keySize, keyFileName);
#endif

	if (open_file (keyFileName, O_RDWR) == -1)
	{
#ifdef DEBUG
		fprintf (stderr, "Could not open file %s\n", keyFileName);
#endif
		errno = KDB_RET_NOTFOUND;
		goto fileerror;
	}
	
	while ((pos=read_key (key, keyRoot)) == 0)
	{
		if (strcmp (key->key, keyFullName) == 0) 
		{	/* right Key found*/
#ifdef DEBUG
			fprintf (stderr, "Key found\n");
			fprintf(stderr, "Name: (%s), Value: (%s), Comment: (%s)\n",
				keyStealName (setKey), (char *) keyStealValue(setKey),
				(char *) keyStealComment (setKey));
#endif
			/*switch to next key*/
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);

			if (op == SETKEY_SET)
			{
				write_key(setKey, oldpos);
			} else {
				remove_key (setKey, oldpos);
			}
			pos = 1;
			break;
		}
		oldpos = ftell (fc);
	}
	if (pos != 1) {	/* key not found, add to the end*/
#ifdef DEBUG
		fprintf (stderr, "Key not found!\n");
#endif
		fseek (fc, 0, SEEK_END);
		oldpos = ftell(fc);
		write_key(setKey, oldpos);
		pos = 0;
	} else if (pos == 1) { /* key found, everything went ok!*/
		pos = 0;
	}
	
	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	return pos; /* success */
memerror:
	errno=KDB_RET_NOMEM;
	close_file ();
#ifdef DEBUG
	fprintf (stderr, "Memory Error\n");
#endif
fileerror:
	
	free (keyFullName);
	free (keyRoot);
	
	keyClose (setKey);
	keyClose (key);
	
	return -1;
}


/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup ini
 */
int kdbRemoveKey_ini(KDBHandle handle, const Key *key) {
	/*return IniSetKey (key, SETKEY_DELETE);*/
	return 0;
}

/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup ini
 */
int kdbRename_ini(KDBHandle handle, Key *key, const char *newName) {
	/*IniSetKey (key, SETKEY_DELETE);
	keySetName (key, newName);
	IniSetKey (key, SETKEY_SET);*/
	return 0; /* success */
}


/**
 * Implementation for kdbStatKey() method.
 * 
 * Trys to stat the file and fill the key
 * with information about that. keys inside
 * a file get the stat information of their
 * file.
 *
 * @ingroup ini
 */
int kdbStatKey_ini(KDBHandle handle, Key *key) {
	char filename [MAX_PATH_LENGTH];
	
	file_name(key, filename);
	stat_file (key, filename);

#ifdef DEBUG
	fprintf (stderr, "kdbStatKey, filename: %s\n", filename);
#endif
	return 0; /* success */
}



/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup ini
 */
uint32_t kdbMonitorKeys_ini(KDBHandle handle, KeySet *interests, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Optional method not implemented\n");
	return 0;
}


/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup ini
 */
uint32_t kdbMonitorKey_ini(KDBHandle handle, Key *interest, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Optional method not implemented\n");
	return 0;
}


/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup ini
 */
KDBEXPORT(ini)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_ini,
		KDB_BE_CLOSE,          &kdbClose_ini,
		KDB_BE_GETKEY,         &kdbGetKey_ini,
		KDB_BE_SETKEY,         &kdbSetKey_ini,
		KDB_BE_STATKEY,        &kdbStatKey_ini,
		KDB_BE_RENAME,         &kdbRename_ini,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_ini,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_ini,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_ini,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_ini,
		KDB_BE_SETKEYS,        &kdbSetKeys_ini,
		KDB_BE_END);
}
