/***************************************************************************
            temaple.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : 01.03.2005
    copyright            : (C) 2005 by Markus Raab
    email                : debian@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *
 *   This is a ini-style backend.
 *   Key/Value Pairs are stored in files in following scheme:
 *   
 *   key1=value1;comment
 *
 * TODO:
 *   create new files and folders
 *   Filelock does not work over NFS. (use fcntl instead of flock)
 *   make \n\t engine
 *   setting errno properly
 *   remove debugging features
 *
 * EXTRA:
 *   include statement (like linking)
 *   monitor/stat files (in which the keys are)
 *   rename/remove keys features
 *
 ***************************************************************************/

#include <kdb.h>
#include <kdbbackend.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <dirent.h>

#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>

#define BACKENDNAME "ini"



/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif _POSIX_PATH_MAX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
/**Fallback: This value should be useful*/
#define MAX_PATH_LENGTH 4096
#endif


/**This buffer size is fastest for reading and writing
 * in files*/
#define BUFFER_RDWR_SIZE 8024


/**Global variables*/
FILE * fc;
int fd;


/**
 * @defgroup backend Elektra framework for pluggable backends
 * @brief The tactics to create pluggable backends to libkdb.so
 *
 * Since version 0.4.9, Elektra can dynamically load different key storage
 * backends. Fast jump to kdbBackendExport() to see an example of a backend
 * implementation.
 * 
 * The methods of class KeyDB that are backend dependent are kdbOpen(),
 * kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename(). So a backend must
 * reimplement these methods.
 * 
 * And methods that have a builtin default high-level inefficient
 * implementation are kdbSetKeys(), kdbMonitorKey(), kdbMonitorKeys(). So
 * it is suggested to reimplement them too, to make them more efficient.
 *
 * The other KeyDB methods are higher level. They use the above methods to
 * do their job, and generally don't have to be reimplemented for a
 * different backend.
 * 
 * The backend must implement a method with name kdbBackendFactory() and no
 * parameters, that is responsible of exporting the implementations of 
 * libkdb.so backend dependent methods.
 * 
 * The backend implementation must:
 * @code
#include <kdb.h>
#include <kdbbackend.h>
 * @endcode
 * 
 * <b>Better than that, a skeleton of a backend implementation is provided inside
 * Elektra development package or source code tree, and should be used as a
 * base for the implementation.</b>
 * 
 * An elektrified program will use the backend defined by environment variable
 * @e $KDB_BACKEND, The backend library is dynamically loaded when the program
 * calls kdbOpen(), unless if the program is security/authentication/setuid
 * related, in which it probably uses the more secure kdbOpenDefault() which
 * completely ignores the @e $KDB_BACKEND environment and will use the
 * @c "default" named backend defined by the sysadmin. Look at
 * @c /lib/libkdb-default.so link to see the default backend for your
 * system.
 * 
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend, and we'll document this skeleton here.
 * 
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libkdb.so look for its library as @c libkdb-BACKENDNAME.so.
 * 
 * Elektra source code tree includes several backend implementations
 * (http://germane-software.com/repositories/elektra/trunk/src/backends)
 * that can also be used as a reference.
 */





/**
 * Initialize the backend.
 * This is the first method kdbOpenBackend() calls after dynamically loading
 * the backend library.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbOpenBackend()
 * @see kdbOpen()
 * @ingroup backend
 */
int kdbOpen_ini() {
	fprintf (stderr, "Open Backend ini\n");
	return 0;
}




/**
 * All finalization logic of the backend should go here.
 * 
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 * Should free any memory that the backend no longer needs.
 * After this call, libkdb.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_ini() {
	fprintf (stderr, "Close Backend ini\n");
	return 0; /* success */
}



/**
 * Implementation for kdbStatKey() method.
 * 
 * @see kdbStatKey() for expected behavior.
 *
 * @ingroup backend
 */
int kdbStatKey_ini(Key *key) {
	//TODO
	fprintf (stderr, "Stat key\n");
	return 0; /* success */
}


/**
 * Opens a file filename.
 * The mode might be O_RDONLY or O_RDWR.
 * @return 0 on success, -1 on failure
 * @ingroup backend*/
int open_file (char * filename, int mode)
{
	char buffer [2] = "\0\0";
	
	if (mode == O_RDWR)
	{
		buffer[0] = 'r';
		buffer[1] = '+';
	}
	else if (mode == O_RDONLY)
	{
		buffer[0] = 'r';
	} else {
		fprintf (stderr, "Mode not useful\n");
		return -1;
	}

	fd = open (filename, mode);
	if (fd == -1) {
		fprintf (stderr, "Unable to open file\n");
		return -1;
	}
	
	if (flock (fd, LOCK_EX) == -1) {
		fprintf (stderr, "Unable to lock file\n");
		return -1;
	}
		
	
	fc = fdopen (fd,buffer);
	if (fc == NULL) {
		fprintf (stderr, "fdopen() failed\n");
		return -1;
	}
	return 0;
}

/**Close previous with open_file() opened file
 * @return 0 on success, -1 on failure*/
int close_file ()
{
	int ret;
	
	if (flock (fd, LOCK_UN) == -1) {
		perror ("Unable to unlock file");
		return -1;
	}

	ret = fclose (fc); // close file
	if (ret != 0) {
		perror ("Could not close file");
		return -1;
	}
	return 0;
}


/**Enlarges file on place where with space bytes. The new
 * place will contain the previous text. The text before
 * where will not be touched.*/
int enlarge_file (long where, long space)
{
	char buffer [BUFFER_RDWR_SIZE+1];
	size_t sread;
	long diff = 0;
	int err;
	int finished = 0;
	long pos;

	fseek (fc,0,SEEK_END); // begin at end
	pos = ftell (fc);
	do {
		pos -= BUFFER_RDWR_SIZE;
		if (pos < where) {
			diff = where - pos;
			pos = where;
			finished = 1;
		}
		fseek (fc, pos, SEEK_SET);
		sread = fread (buffer,1,BUFFER_RDWR_SIZE-diff,fc);	// read last peace
		buffer[sread] = 0;	// mark end (not necessary)

		fseek (fc,pos+space,SEEK_SET);	// jump to writepos

		printf ("buffer: %s, sread: %d\n", buffer, sread);
		fwrite (buffer,1,sread,fc);
		err = ferror (fc);
		if (err != 0)
		{
			fprintf (stderr, "Error in stream\n");
			return -1;
		}
	} while (! finished);

	return 0;
}

/**Shrinks file on place where with space bytes.
 * The old text (length space after where) will 
 * be lost! The text before where will not be touched.*/
int shrink_file (long where, long space)
{
	char buffer [BUFFER_RDWR_SIZE+1];
	size_t sread;
	int err;
	long pos;

	fseek (fc,where, SEEK_SET);
	pos = ftell (fc);
	
	do {
		fseek (fc,pos+space,SEEK_SET); // jump to readposition
		sread = fread (buffer,1,BUFFER_RDWR_SIZE,fc);	// read a peace
		buffer[sread] = 0;	// mark end (not necessary)

		fseek (fc,pos,SEEK_SET);	// jump to writepos

		printf ("buffer: %s, sread: %d\n", buffer, sread);
		fwrite (buffer,1,sread,fc);
		err = ferror (fc);
		if (err != 0)
		{
			fprintf (stderr, "Error in stream\n");
			return -1;
		}
		pos += sread;
	} while (sread == BUFFER_RDWR_SIZE);

	ftruncate (fd,lseek(fd,0,SEEK_CUR));
	
	return 0;
}

/**
 * Get the filename for the Key forKey.
 * The filename has 2 parts:
 *
 * /etc/kdb/system/path/to/key
 *
 * /etc/kdb/system is the path of the namespace.
 * There are 2 possibilites. It may be KEY_NS_SYSTEM
 * or KEY_NS_USER. When the key of a user is asked
 * for, then environment will be asked what USER is
 * logged on.
 * 
 * @ingroup backend
 */
int IniGetName (const Key * forKey, char * filename)
{
	size_t length;
	int namesize;
	char * name;

        switch (keyGetNamespace(forKey)) {
                case KEY_NS_SYSTEM: {
                        /* Prepare to use the 'system/ *' database */
                        strncpy(filename,KDB_DB_SYSTEM,MAX_PATH_LENGTH);
                        length=strlen(filename);
                        break;
                }
                case KEY_NS_USER: {
                        /* Prepare to use the 'user:????/ *' database */
                        struct passwd *user=0;

                        if (forKey->userDomain) user=getpwnam(forKey->userDomain);
                        else user=getpwnam(getenv("USER"));

                        if (!user) return 0; /* propagate errno */
                        length=snprintf(filename,MAX_PATH_LENGTH,"%s/%s",user->pw_dir,KDB_DB_USER);
                        break;
                }
                default: {
                        errno=KDB_RET_INVALIDKEY;
                        return 0;
                }
        }
	
        filename[length]='/'; length++; // now the Keyname follows

	namesize = keyGetNameSize (forKey);
	if (namesize == 0) return 0;
	name = (char*) malloc (namesize);

	keyGetName (forKey, name, namesize);

	if (length > MAX_PATH_LENGTH) return -1;	// too long

	strncpy (filename + length, name, namesize);
	length += namesize;
	free (name);
	
	return length;
}

/**
 * Splits the IniGetName into two parts.
 * 
 * @ingroup backend
 */
int IniGetFileName (const Key * forKey, char * filename, char * keyname)
{
	int length;
	char * end;

	length = IniGetName (forKey, filename);
	
	end = strrchr (filename, '/');	// dirname
	*end = 0;
	strncpy (keyname, end+1, MAX_PATH_LENGTH);
	
	return length;
}

#define STATE_KEY 1
#define STATE_VALUE 2
#define STATE_COMMENT 4
#define STATE_END 8

int count = 0;

/**Debug: Define very small Buffer Size to make
 * sure that realloc works!*/
// #define BUFFER_SIZE 4

/**
 * Read one key out of a file.
 *
 * It does not check the Name of the Key
 * 
 * @ingroup backend
 */
int IniGetKey (Key * key, char * root)
{
	char * buffer;
	char * buffer_value;
	char * buffer_key;
	char * buffer_comment;
	char * buffer_name;
	int rc;
	
	int i;
	int state = STATE_KEY;	// start reading the key
	
	int string_length = BUFFER_SIZE;
	int value_length = BUFFER_SIZE;
	int key_length = BUFFER_SIZE;
	int comment_length = BUFFER_SIZE;
	
	int v=0;	// position of value
	int k=0;	// position of key
	int c=0;	// position of comment
	
	buffer = (char*) malloc (BUFFER_SIZE+1);
	
	if (fgets (buffer, BUFFER_SIZE,fc) == NULL) {
		fprintf (stderr, "End of File\n");
		free (buffer);
		return -1;
	}
	
	buffer_value = (char*) malloc (BUFFER_SIZE+1);
	buffer_key = (char*) malloc (BUFFER_SIZE+1);
	buffer_comment = (char*) malloc (BUFFER_SIZE+1);
	
	for (i=0; i < string_length; i++) {
//		fprintf (stderr, "Processing |%c|%d|\n", buffer[i], buffer[i]);
		if (buffer[i] == '\n') { // end of line found
//			fprintf (stderr, "Found end of key (\\n)\n");
			break;
		}
		else if (buffer[i] ==  '\0' ) {	// anticipated end?
			if (i==string_length-1) { // no its not
				string_length += BUFFER_SIZE;
				if (srealloc ((void**) & buffer, string_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer_value);
					free (buffer_key);
					free (buffer_comment);
					return -1;
				}
				fgets (buffer+string_length-BUFFER_SIZE,
					BUFFER_SIZE,fc);
			} else {
				fprintf (stderr, "No Enter found in this line?\n");
				return -1;
			}
		}
		else if (buffer[i] == '=') {	// value follows
			state = STATE_VALUE;
		}
		else if (buffer[i] == ';') {	// comment follows
			state = STATE_COMMENT;
		}
		else if (state == STATE_KEY) {
			buffer_key [k++] = buffer[i];
			if (k == key_length-1)
			{
				key_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_key, key_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_value);
					free (buffer_comment);
					return -1;
				}					
			}
		}
		else if (state == STATE_VALUE) {
			buffer_value [v++] = buffer[i];
			if (v == value_length-1) 
			{
				value_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_value, value_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_key);
					free (buffer_comment);
					return -1;
				}					
			}
		}
		else if (state == STATE_COMMENT) {
			buffer_comment [c++] = buffer[i];
			if (c == comment_length-1)
			{
				comment_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_comment, comment_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_value);
					free (buffer_key);
					return -1;
				}					
			}
		}
	}

	free (buffer);
	buffer_value [v] = 0;
	buffer_key [k] = 0;
	buffer_comment [c] = 0;	// key eingelesen

	/*if (key->key) free (key->key);
	key->key = malloc (k+1);
	strcpy (key->key, buffer_key);*/

	
	if ((buffer_name = malloc (strlen(buffer_key) + strlen(root) + 2)) == NULL)
	{
		fprintf (stderr, "Allocation error");
		free (buffer_key);
		free (buffer_value);
		free (buffer_comment);
		return -1;
	}
	buffer_name[0] = '\0';	// buffer_name is empty
	strcat (buffer_name, root);
	strcat (buffer_name, "/");
	strcat (buffer_name, buffer_key);
	rc = keySetName (key, buffer_name);
	if (rc==0) {
		fprintf (stderr, "Unable to set name\n");
	} else	fprintf (stderr, "Name set to %s\n", buffer_name);
	free (buffer_name);
	
	/*if (key->data) free (key->data);	
	key->data = malloc (v+1);
	strcpy (key->data, buffer_value);
	key->dataSize = v+1;*/
	rc = keySetString (key, buffer_value);
	if (rc==0) {
		fprintf (stderr, "Unable to set value\n");
	} else fprintf (stderr, "Value set to %s\n", buffer_value);
	
	/*if (key->comment) free (key->comment);
	key->comment = malloc (c+1);
	strcpy (key->comment, buffer_comment);
	key->commentSize = c+1;*/
	rc = keySetComment (key, buffer_comment);
	if (rc==0) {
		fprintf (stderr, "Unable to set comment\n");
	} else fprintf (stderr, "Comment set to %s\n", buffer_comment);
	
	/*key->type = KEY_TYPE_STRING;
	key->recordSize = v+c+2;*/

	// remove sync flag
	key->flags &= ~KEY_SWITCH_NEEDSYNC;

	free (buffer_value);
	free (buffer_key);
	free (buffer_comment);
	
	return 0; /* success */
}

/**
 * Implementation for kdbGetKey() method.
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
int kdbGetKey_ini(Key *key) {
	char keyFileName [MAX_PATH_LENGTH];
	char keyName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName;
	char * keyRoot;
	char * end;
	
	fprintf (stderr, "kdbGetKey_ini() entered\n");
	
	pos = IniGetFileName(key, keyFileName, keyName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	// dirname
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	strcpy (keyRoot, keyFullName);
	*end = '/';
	
	fprintf (stderr, "keyRoot: %s\n", keyRoot);

	if (! pos) {
		fprintf (stderr, "Could not receive filename");
		return -1;
	}
	fprintf (stderr, "Get Key %s [%d] in File: %s\n", keyName, keySize, keyFileName);

	open_file (keyFileName, O_RDONLY);
	
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		fprintf (stderr, "Compare: %s with %s\n", key->key, keyFullName);
		if (strcmp (key->key, keyFullName) == 0) {	// right Key found
			//TODO: use keySetName (key, keyFullName);
			//or DONT EVEN SET, because key->key and keyFullName is the same
			fprintf (stderr, "Key found\n");
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);
	
			fprintf (stderr, "key: %s, value: %s, comment: %s\n", 
				key->key, (char *) key->data, key->comment);
		
			
			pos = 1;
			break;
		}
	}
	if (pos != 1) {	// key not found, leave it, so that app won't sigfault
		errno = KDB_RET_NOKEY;
		pos = -1;
		fprintf (stderr, "Key not found!\n");
	} else if (pos == 1) { // key found, everything went ok!
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
 * @ingroup backend
 */
ssize_t kdbGetKeys (char * keyFileName, char * keyRoot, KeySet * returned)
{
	Key * key;
	int pos;

	open_file (keyFileName, O_RDONLY);
	key = keyNew (KEY_SWITCH_END);

	fprintf (stderr, "Call IniGetKey(%s)\n", keyRoot);
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		fprintf (stderr, "Append key\n");
		ksAppend (returned,key);

		key = keyNew (KEY_SWITCH_END);
	}
	
	keyDel (key); // delete the not used key left

	close_file();
	
	return 0; /* success */
}

/**
 * Writes out a key into file on pos.
 * keySet is the key which should be written there
 *
 * @ret Returnes 0 on success.
 * 
 * @ingroup backend
 */
int IniWriteKey (Key * keySet, long pos)
{
	return 0;
}


/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_ini(const Key * key, KeySet *returned, unsigned long options) {
	char pathName [MAX_PATH_LENGTH];
	char fileFullName [MAX_PATH_LENGTH];
	char * keyName;
	size_t keyLength;
	char * keyRoot;
	size_t keyRootLength;
	int ret;
	DIR * dir;
	struct dirent * filename;

	ret = IniGetName (key, pathName);
	
	if (ret == -1) {
		fprintf (stderr, "Error, could not get FileName\n");
		return -1;
	}

	fprintf (stderr, "Pathname: %s, Keyname: %s\n", pathName, keyName);

	//TODO: it might be a file/section!
	dir = opendir (pathName);
	if (dir == NULL) {
		fprintf (stderr, "Could not open directory %s", pathName);
		return -1;
	}
	
	while ((filename = readdir (dir)))
	{
		if (	strcmp(filename->d_name, ".")  == 0 || 
			strcmp(filename->d_name, "..") == 0)
			continue;
		
		if (filename->d_name[0] == '.' && !(options & KDB_O_INACTIVE))
			continue;
	
		keyLength = keyGetNameSize (key);
		keyRootLength= keyLength + strlen (filename->d_name) + 1;
		keyRoot = malloc (keyRootLength);
		keyName = malloc (keyLength);
		keyGetName (key, keyName, keyGetNameSize (key));
		strcat (keyRoot, keyName);
		strcat (keyRoot, "/");
		strcat (keyRoot, filename->d_name);

		fileFullName[0] = 0;	// delete old Name
		strncat (fileFullName, pathName, MAX_PATH_LENGTH);
		strncat (fileFullName, "/", MAX_PATH_LENGTH);
		strncat (fileFullName, filename->d_name, MAX_PATH_LENGTH);
		fprintf (stderr, "Call kdbGetKeys(fileFullName: %s, keyRoot: %s,returned)\n", 
			fileFullName, keyRoot);
		kdbGetKeys (fileFullName, keyRoot, returned);
		
		free (keyRoot);
		free (keyName);
	}

	closedir (dir);

	free (keyName);

	fprintf (stderr, "Leaving (ret: %d)\n", returned->size);
	
	return returned->size; /* success */
}


/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_ini(Key *key) {
	char keyFileName [MAX_PATH_LENGTH];
	char keyName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName;
	char * keyRoot;
	char * end;

	Key * setKey;
	long oldpos, newpos;
	int needed_size;

	setKey = keyNew (KEY_SWITCH_END);
	keyDup (key, setKey);	// clone key
	
	fprintf (stderr, "kdbGetKey_ini() entered\n");
	
	pos = IniGetFileName(key, keyFileName, keyName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	// dirname
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	strcpy (keyRoot, keyFullName);
	*end = '/';
	
	fprintf (stderr, "keyRoot: %s\n", keyRoot);

	if (! pos) {
		fprintf (stderr, "Could not receive filename");
		return -1;
	}
	fprintf (stderr, "Set Key %s [%d] in File: %s\n", keyName, keySize, keyFileName);

	open_file (keyFileName, O_RDWR);
	
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		if (strcmp (key->key, keyFullName) == 0) {	// right Key found
			//TODO: use keySetName (key, keyFullName);
			//or DONT EVEN SET, because key->key and keyFullName is the same
			fprintf (stderr, "Key found\n");
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);

			// use setkey to set the key to wished values
			newpos = ftell (fc);
			end = strrchr (keyStealName (setKey),'/')+1;
			needed_size = strlen (end) +// =
				keyGetDataSize (setKey) +	// ;
				keyGetCommentSize (setKey) + 1;	// \n
			if (newpos - oldpos > needed_size)
			{
				fprintf (stderr, "Shrinking File with %ld bytes",
						newpos - oldpos - needed_size);
				shrink_file (oldpos, newpos - oldpos -needed_size);
			} else if (newpos - oldpos < needed_size) {
				fprintf (stderr, "Enlarge File with %ld bytes",
						needed_size - (newpos - oldpos));
				enlarge_file (newpos, needed_size - (newpos - oldpos));
			}
			
			fprintf(stderr, "Writing key to disc (pos: %ld|%ld|%d) ...\n",
				oldpos, newpos, needed_size);
			fseek (fc, oldpos, SEEK_SET);
			
			fwrite (       end, strlen(end), 1, fc);
			fwrite ("=", 1,1,fc);
			fwrite (keyStealValue (setKey), keyGetValueSize(setKey)-1, 1, fc);
			fwrite (";", 1,1,fc);
			fwrite (keyStealComment (setKey), keyGetCommentSize(setKey)-1, 1, fc);
			fwrite ("\n", 1,1,fc);

			newpos = ftell (fc);
			fprintf (stderr, "Real endpos: %ld\n", newpos);
	
			fprintf (stderr, "key: %s, value: %s, comment: %s\n", 
				setKey->key, (char *) setKey->data, setKey->comment);
		
			
			pos = 1;
			break;
		}
		oldpos = ftell (fc);
	}
	if (pos != 1) {	// key not found, leave it, so that app won't sigfault
		errno = KDB_RET_NOKEY;
		pos = -1;
		fprintf (stderr, "Key not found!\n");
	} else if (pos == 1) { // key found, everything went ok!
		pos = 0;
	}
	
	// ftruncate file

	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	return pos; /* success */
}


/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_ini(Key *key, const char *newName) {
	fprintf (stderr, "Give Key a new Name\n");
	return 0; /* success */
}

/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_ini(const Key *key) {
	fprintf (stderr, "Remove Key from Database\n");
	return 0;  /* success */
}





/**
 * Implementation for kdbSetKeys() method.
 * 
 * The implementation of this method is optional, and a builtin, probablly 
 * inefficient implementation can be explicitly used when exporting the
 * backend with kdbBackendExport(), using kdbSetKeys_default().
 * 
 * @see kdbSetKeys() for expected behavior.
 * @ingroup backend
 */
int kdbSetKeys_ini(KeySet *ks) {
	fprintf (stderr, "Set many Keys at once\n");
	return 0;
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKeys_ini(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Monitor many Keys at once\n");
	return 0;
}



/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKey_ini(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Monitor a key\n");
	return 0;
}


/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libkdb.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup backend
 */
KDBBackend *kdbBackendFactory(void) {
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
		/* set to default implementation: */
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}
