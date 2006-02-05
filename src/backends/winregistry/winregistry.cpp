// winregistry.cpp : Defines the entry point for the DLL application.
// $Id$

#include "winregistry.h"
#include <windows.h>

int mapError(LONG err);

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}



/**
 * @defgroup backend Elektra framework for pluggable backends
 * @brief The tactics to create pluggable backends to libelektra.so
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
 * libelektra.so backend dependent methods.
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
 * @c /lib/libelektra-default.so link to see the default backend for your
 * system.
 * 
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend, and we'll document this skeleton here.
 * 
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libelektra.so look for its library as @c libelektra-BACKENDNAME.so.
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
 * This method is responsible of:
 * - backend's specific configuration gathering
 * - all backend's internal structs initialization
 * - initial setup of all I/O details such as opening a file, connecting to a
 *   database, etc
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbOpenBackend()
 * @see kdbOpen()
 * @ingroup backend
 */
int kdbOpen_winregistry() {
	/* backend initialization logic */
	return 0;
}




/**
 * All finalization logic of the backend should go here.
 * 
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 * Should free any memory that the backend no longer needs.
 * After this call, libelektra.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_winregistry() {
	/* free all backend resources and shut it down */
	return 0; /* success */
}



/**
 * Implementation for kdbStatKey() method.
 *
 * This method is responsible of:
 * - make necessary I/O to retrieve @p key->name's metadata
 * - fill the @p key struct with its metadata
 *
 * @see kdbStatKey() for expected behavior.
 * @ingroup backend
 */
int kdbStatKey_winregistry(Key *key) {
	/* get the most possible key metainfo */
	return 0; /* success */
}


/**
 * Implementation for kdbGetKey() method.
 *
 * This method is responsible of:
 * - make necessary I/O to retrieve all @p key->name's value and metadata
 * - fill the @p key struct with its value and metadata
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
int kdbGetKey_winregistry(Key *key) {
	/* fully gets a key */
	HKEY rootKey = {0};
	HKEY desiredKey = {0};
	char path[1024];
	DWORD type;
	char val[1024];
	DWORD sizeval = sizeof(val);
	char *keyName = NULL;
	LONG err=0;
	if(!kdbGetRegistryPath(key, rootKey, path, sizeof(path), &keyName))
	{
		return -1;
	}
	
	if(err = RegGetValue(rootKey, path, keyName, RRF_RT_REG_SZ|RRF_RT_REG_MULTI_SZ, &type, val, &sizeval) != ERROR_SUCCESS)
	{
		errno = mapError(err);
		return -1;
	}
	keySetType(key,KEY_TYPE_STRING);
	keySetRaw(key, val, strlen(val));
	/*if((err = RegOpenKeyEx(rootKey, path, 0, KEY_READ,&desiredKey)) != ERROR_SUCCESS)
	{
		/* Error opening key 
		errno = mapError(err);
		return -1;
	}
	if(RegQueryValueEx(desiredKey, keyName, 0,&type, (LPBYTE)val, &sizeval) != ERROR_SUCCESS)
	{
		errno = mapError(err);
		return -1;
	}*/
	


	return 0; /* success */
}



/**
 * Implementation for kdbSetKey() method.
 *
 * This method is responsible of:
 * - check the existence of @p key->name on persistent storage
 * - prepare the backend to receive a new or updated key
 * - use value and metadata from @p key to store them in the backend storage
 * - fill the @p key struct with its value and metadata
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_winregistry(Key *key) {
	/* fully sets a key */
	HKEY rootKey = {0};
	HKEY desiredKey = {0};
	char path[1024];
	DWORD type;
	char *keyName = NULL;
	LONG err=0;
	if(!kdbGetRegistryPath(key, rootKey, path, sizeof(path), &keyName))
	{
		return -1;
	}
	if((err = RegOpenKeyEx(rootKey, path, 0, KEY_READ,&desiredKey)) != ERROR_SUCCESS)
	{
		/* Error opening key */
		errno = mapError(err);
		return -1;
	}
	/* TODO: NOT DELETE VALUE HERE!!! */
	if((err = RegSet(?)Value(desiredKey, keyName)) != ERROR_SUCCESS)
	{
		errno = mapError(err);
		RegCloseKey(desiredKey);
		return -1;
	}
	RegCloseKey(desiredKey);
	return 0;  /* success */
}



/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_winregistry(Key *key, const char *newName) {
	/* rename a key to another name */
	return 0; /* success */
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_winregistry(const Key *key) {
	/* remove a key from the database */
	HKEY rootKey = {0};
	HKEY desiredKey = {0};
	char path[1024];
	DWORD type;
	char *keyName = NULL;
	LONG err=0;
	if(!kdbGetRegistryPath(key, rootKey, path, sizeof(path), &keyName))
	{
		return -1;
	}
	if((err = RegOpenKeyEx(rootKey, path, 0, KEY_READ,&desiredKey)) != ERROR_SUCCESS)
	{
		/* Error opening key */
		errno = mapError(err);
		return -1;
	}
	if((err = RegDeleteValue(desiredKey, keyName)) != ERROR_SUCCESS)
	{
		errno = mapError(err);
		RegCloseKey(desiredKey);
		return -1;
	}
	RegCloseKey(desiredKey);
	return 0;  /* success */
}




/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_winregistry(const Key *parentKey, KeySet *returned, unsigned long options) {
	/* retrieve multiple hierarchical keys */
	return (ssize_t)returned->size; /* success */
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
int kdbSetKeys_winregistry(KeySet *ks) {
	/* set many keys */
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
uint32_t kdbMonitorKeys_winregistry(KeySet *interests, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
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
uint32_t kdbMonitorKey_winregistry(Key *interest, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	return 0;
}

KDBEXPORT(BACKENDNAME) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_winregistry,
		KDB_BE_CLOSE,          &kdbClose_winregistry,
		KDB_BE_GETKEY,         &kdbGetKey_winregistry,
		KDB_BE_SETKEY,         &kdbSetKey_winregistry,
		KDB_BE_STATKEY,        &kdbStatKey_winregistry,
		KDB_BE_RENAME,         &kdbRename_winregistry,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_winregistry,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_winregistry,
		KDB_BE_END);
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
size_t kdbGetRegistryPath(const Key *forKey,HKEY rootkey, char *path, size_t maxSize, char **keyName) {
	size_t length=0;
	char *sid = NULL;

	switch (keyGetNamespace(forKey)) {
		case KEY_NS_SYSTEM: 
			/* Prepare to use the 'system/ *' database */
			rootkey = HKEY_LOCAL_MACHINE;
			length = snprintf(path, maxSize, "%s/%s", KDB_REGISTRY_PATH, forKey->key);
			break;

		case KEY_NS_USER: 
			/* Prepare to use the 'user:????/ *' database */
			if (forKey->userDomain) 
			{
				/* This is unsupported so far */
				errno = KDB_RET_INVALIDKEY;
				return 0;
				/* FIXME: The Following code should work immediately as soon as getSID is implemented */
				/*rootkey = HKEY_USERS;
				sid=getSID(forKey->userDomain);
				if (!sid) return 0; /* propagate errno 
				length = snprintf(path,maxSize, "%s/%s/%s", sid, KDB_REGISTRY_PATH, forKey->key);*/
			}
			else
			{
				rootkey = HKEY_CURRENT_USER;
				length = snprintf(path, maxSize, "%s/%s", KDB_REGISTRY_PATH, forKey->key);
			}
			break;

		default: {
			errno=KDB_RET_INVALIDKEY;
			return 0;
		}
	}
	*keyName = strrchr(path, '/');
	*keyName = '\0';
	*keyName++;
	return length;
}

/* getSID finds the Security identifier of a given username. This is needed to use user:username/ elektra urls */
size_t getSID(char *username, char *sid, int size)
{
	return 0;
}

int mapError(LONG err)
{
	switch(err)
	{
	case ERROR_ACCESS_DENIED: return KDB_RET_NOCRED;
	default:
		return KDB_RET_NOTFOUND;
	}
}

