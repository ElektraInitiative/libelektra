/***************************************************************************
            temaple.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : 01.03.2005
    copyright            : (C) 2005 by Markus Raab
    email                : mail@markus-raab.org
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
 *   Filelock does not work over NFS. (use fcntl instead of flock)
 *   make \n\t engine
 *   there is no multiple fetching keys, even it could be done very fast
 *   setting keys does not work
 *   setting errno properly
 *   remove debugging features
 *
 * EXTRA:
 *   include statement (like linking)
 *   monitor/stat files (in which the keys are)
 *   rename/remove keys features
 *
 ***************************************************************************/


/* Subversion stuff

$Id: template.c 173 2005-01-26 01:24:26Z aviram $
$LastChangedBy: aviram $

*/



#include <kdb.h>
#include <kdbbackend.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>

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
	printf ("Open Backend ini\n");
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
	printf ("Close Backend ini\n");
	return 0; /* success */
}



/**
 * Implementation for kdbStatKey() method.
 * 
 * @see kdbStatKey() for expected behavior.
 * @ingroup backend
 */
int kdbStatKey_ini(Key *key) {
	//TODO
	printf ("Stat key\n");
	return 0; /* success */
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
int IniGetFileName (Key * forKey, char * filename, char * keyname)
{
	char * name;
	char * end;
	size_t namesize;
	size_t length;

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
        }	// system/user part done

        filename[length]='/'; length++; // now the Keyname follows

	namesize = keyGetNameSize (forKey);
	if (namesize == 0) return 0;
	name = (char*) malloc (namesize);

	keyGetName (forKey, name, namesize);

	if (length > MAX_PATH_LENGTH) return -1;	// too long

	strncpy (filename + length, name, namesize);
	length += namesize;

	end = strrchr (filename, '/');	// dirname
	*end = 0;
	strncpy (keyname, end+1, MAX_PATH_LENGTH);
	
	return length;
}

#define STATE_KEY 1
#define STATE_VALUE 2
#define STATE_COMMENT 4
#define STATE_END 8

/**
 * Read one key out of a file.
 *
 * It does not check the Name of the Key
 * 
 * @ingroup backend
 */
int IniGetKey (FILE * fc, Key * key)
{
	char * buffer;
	char * buffer_value;
	char * buffer_key;
	char * buffer_comment;
	
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
	buffer_value = (char*) malloc (BUFFER_SIZE+1);
	buffer_key = (char*) malloc (BUFFER_SIZE+1);
	buffer_comment = (char*) malloc (BUFFER_SIZE+1);
	
	if (fgets (buffer, BUFFER_SIZE,fc) == NULL) {
		printf ("End of File\n");
		return -1;
	}
	
	for (i=0; i < string_length; i++) {
	//	fprintf (stderr, "Processing |%c|%d|\n", buffer[i], buffer[i]);
		if (buffer[i] == '\n') { // end of line found
			fprintf (stderr, "Found end of key\n");
			break;
		}
		else if (buffer[i] ==  '\0' ) {	// anticipated end?
			if (i==string_length-1) { // no its not
				string_length += BUFFER_SIZE;
				printf (".");
				buffer = realloc (buffer, string_length);
				if (buffer == NULL) {
					printf ("Reallocation error\n");
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
				buffer_key = realloc (buffer_key, key_length);
				printf ("k");
				if (buffer_key == NULL) {
					printf ("Reallocation error\n");
					return -1;
				}					
			}
		}
		else if (state == STATE_VALUE) {
			buffer_value [v++] = buffer[i];
			if (v == value_length-1) 
			{
				value_length += BUFFER_SIZE;
				buffer_value = realloc (buffer_value, value_length);
				printf ("v");
				if (buffer_value == NULL) {
					printf ("Reallocation error\n");
					return -1;
				}					
			}
		}
		else if (state == STATE_COMMENT) {
	//		fprintf (stderr, "Comment |%c|%d|\n", buffer[i], buffer[i]);
			buffer_comment [c++] = buffer[i];
			if (c == comment_length-1)
			{
				comment_length += BUFFER_SIZE;
				buffer_comment = realloc (buffer_comment, comment_length);
				printf ("c");
				if (buffer_comment == NULL) {
					printf ("Reallocation error\n");
					return -1;
				}					
			}
		}
	}

	buffer_value [v] = 0;
	buffer_key [k] = 0;
	buffer_comment [c] = 0;	// key eingelesen

	
	if (key->data) free (key->data);	
	key->data = malloc (v+1);
	strcpy (key->data, buffer_value);
	key->dataSize = v+1;
	
	if (key->comment) free (key->comment);
	key->comment = malloc (c+1);
	strcpy (key->comment, buffer_comment);
	key->commentSize = c+1;

	if (key->key) free (key->key);
	key->key = malloc (k+1);
	strcpy (key->key, buffer_key);
	
	key->type = KEY_TYPE_STRING;

	key->recordSize = v+c+2;

	key->flags &= ~KEY_SWITCH_NEEDSYNC;

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
	FILE * fc; int fd;	// filedescriptor
	
	pos = IniGetFileName(key, keyFileName, keyName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	strncpy (keyFullName, key->key, keySize);

	if (! pos) {
		printf ("Could not receive filename");
		return -1;
	}
	printf ("Get Key %s [%d] in File: %s\n", keyName, keySize, keyFileName);

	fd = open (keyFileName, O_RDONLY);
	if (fd == -1) {
		printf ("Unable to open file");
		return -1;
	}
	
	if (flock (fd, LOCK_EX) == -1) {
		printf ("Unable to lock file");
	}
	
	fc = fdopen (fd,"r");
	
	while ((pos=IniGetKey (fc, key)) == 0)
	{
//		printf ("Compare: %s with %s\n", readKey->key, keyName);
		if (strcmp (key->key, keyName) == 0) {	// right Key found
			printf ("Key found\n");
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);
	
			printf ("key: %s, value: %s, comment: %s\n", 
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
	
	if (flock (fd, LOCK_UN) == -1) {
		perror ("Unable to unlock file");
	}	
	close (fd); // close filedescriptor
	
	return pos; /* success */
}



/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_ini(Key *key) {
	printf ("Set Key in File\n");
	return 0; /* success */
}



/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_ini(Key *key, const char *newName) {
	printf ("Give Key a new Name\n");
	return 0; /* success */
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_ini(const Key *key) {
	printf ("Remove Key from Database\n");
	return 0;  /* success */
}




/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_ini(const Key * key, KeySet *returned, unsigned long options) {
	printf ("Get many Keys at once\n");	
	return returned->size; /* success */
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
	printf ("Set many Keys at once\n");
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
	printf ("Monitor many Keys at once\n");
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
	printf ("Monitor a key\n");
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
