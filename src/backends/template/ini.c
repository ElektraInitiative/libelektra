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
 *   #comment to key1
 *   key1 = value1
 *
 *   It does not work over NFS.
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
int kdbGetFileName (Key * forKey, char * filename, size_t maxSize)
{
	char * name;
	char * end;
	size_t namesize;
	size_t length;

        switch (keyGetNamespace(forKey)) {
                case KEY_NS_SYSTEM: {
                        /* Prepare to use the 'system/ *' database */
                        strncpy(filename,KDB_DB_SYSTEM,maxSize);
                        length=strlen(filename);
                        break;
                }
                case KEY_NS_USER: {
                        /* Prepare to use the 'user:????/ *' database */
                        struct passwd *user=0;

                        if (forKey->userDomain) user=getpwnam(forKey->userDomain);
                        else user=getpwnam(getenv("USER"));

                        if (!user) return 0; /* propagate errno */
                        length=snprintf(filename,maxSize,"%s/%s",user->pw_dir,KDB_DB_USER);
                        break;
                }
                default: {
                        errno=KDB_RET_INVALIDKEY;
                        return 0;
                }
        }

        filename[length]='/'; length++;

	namesize = keyGetNameSize (forKey);
	if (namesize == 0) return 0;
	name = (char*) malloc (namesize);

	keyGetName (forKey, name, namesize);
	if (length + namesize > maxSize) return -1;	// too long

	strncpy (filename + length, name, namesize);

	end = strrchr (filename, '/');	// dirname
	*end = 0;
	
	return strlen (filename);
}
#define STATE_KEY 1
#define STATE_VALUE 2
#define STATE_ELSE 4
#define STATE_END 8


/**
 * Implementation for kdbGetKey() method.
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
int kdbGetKey_ini(Key *key) {
	char keyFileName [MAX_PATH_LENGTH];
	char buffer [BUFFER_SIZE];
	char buffer_value [BUFFER_SIZE];
	char buffer_key [BUFFER_SIZE];
	int v=0;	// position of value
	int k=0;	// position of key
	int pos;
	int i;
	size_t r;
	int fd;	// filedescriptor
	int state;
	
	pos = kdbGetFileName(key, keyFileName, sizeof(keyFileName));
	printf ("Get Key in File: %s\n", keyFileName);

	if (! pos) {
		printf ("Could not receive filename");
		return -1;
	}

	fd = open (keyFileName, O_RDONLY);
	if (fd == -1) {
		printf ("Unable to open file");
		return -1;
	}
	
	if (flock (fd, LOCK_EX) == -1) {
		printf ("Unable to lock file");
	}
	
	while ((r=read (fd, buffer, BUFFER_SIZE)) != -1)
	{
		state = STATE_KEY;
		k=0; v=0;
		for (i=0; i<r; i++) {
			if (buffer[i] == '\n') {	// end found
				state = STATE_END;
				break;
			}
			else if (buffer[i] == '=') {	// value follows
				state = STATE_VALUE;
			}
			else if (state == STATE_KEY) {
				buffer_key [k++] = buffer[i];
			}
			else if (state == STATE_VALUE) {
				buffer_value [v++] = buffer[i];
			}
		}
		buffer_value [v] = 0;
		buffer_key [k] = 0;
		if (r!= BUFFER_SIZE) break;	// buffer was not full, so end
	}
	printf ("buffer: %s, key: %s, value: %s\n", buffer, buffer_key, buffer_value);
	
	keySetRaw(key,buffer_value,v+1);

	key->flags &= ~KEY_SWITCH_NEEDSYNC;
	return 0; /* success */
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
int kdbGetKeyChildKeys_ini(const Key * key, KeySet *returned, unsigned long options) {
	printf ("Get many Keys at once\n");	
	return 0; /* success */
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
