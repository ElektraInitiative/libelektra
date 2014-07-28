/***************************************************************************
                          keyhelpers.c  -  Methods for Key manipulation
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"



/*
 * Returns one level of the key name.
 *
 * Interface should be build on top of Keys.
 * Interface is not const-correct (it does a const-cast).
 *
 * This method is used to skip repeating '/' and to find escaping chars.
 * Given @p keyName, this method returns a pointer to the next name level
 * found and changes @p size to the number of bytes on this level name.
 *
 * This method is used by keySetName() to cleanup parameters
 * before being accepted in the Key object.
 *
 * @code
// Lets define a key name with a lot of repeating '/' and escaped '/'
char *keyName="user////abc/def\/ghi////jkl///";
char *p=keyName;
size_t size=0;
int level=0;
char buffer[20]; // TODO: make sure buffer size is ok

p=keyName;
while (*(p=keyNameGetOneLevel(p+size,&size)))
{
	level++;

	// copy what we found to a buffer, so we can NULL-terminate it
	strncpy(buffer,p,size);
	buffer[size]=0;

	printf("Level %d name: \"%s\"\n",level,buffer);
}

 * The above example will produce the following output:
 *
 * @code
Level 1 name: user
Level 2 name: abc
Level 3 name: def\/ghi
Level 4 name: jkl
 * @endcode
 *
 * @param name the string that will be searched
 * @param size the number of bytes of level name found in @p keyName until
 * 	the next delimiter ('/')
 * @return a pointer to the first char of the next level name, it will point to
 * 	NULL when done.
 * @ingroup keyname
 */
char *keyNameGetOneLevel(const char *name, size_t *size)
{
	char *real=(char *)name;
	size_t cursor=0;
	int escapeNext=0;
	int end=0;

	/* skip all repeating '/' in the beginning */
	while (*real && *real == KDB_PATH_SEPARATOR)
	{
		real++;
	}

	/* now see where this basename ends handling escaped chars with '\' */
	while (real[cursor] && !end)
	{
		switch (real[cursor])
		{
		case KDB_PATH_ESCAPE:
			escapeNext=1;
			break;
		case KDB_PATH_SEPARATOR:
			if (! escapeNext)
			{
				end=1;
			}
			// fallthrough
		default:
			escapeNext=0;
		}
		++cursor;
	}

	/* if a '/' stopped our loop, balance the counter */
	if (end)
	{
		--cursor;
	}

	*size=cursor;
	return real;
}




/*
 * Gets number of bytes needed to store root name of a key name
 *
 * Possible root key names are @p system, @p user or @p "user:someuser" .
 *
 * @return number of bytes needed with ending NULL
 * @param keyName the name of the key
 * @see keyGetFullRootNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetFullRootNameSize(const char *name)
{
	char *end;
	int length=strlen(name);

	if (!length) return 0;

	/*
		Possible situations:
		user:someuser
		user:someuser/
		user:someuser/key/name
		user:some.user/key/name
		.
		\.
		(empty)
	*/
	end=strchr(name,KDB_PATH_SEPARATOR);
	if (!end) /* Reached end of string. Root is entire key. */
		end = (char *)name + length;

	return end-name+1;
}



/*
 * Gets number of bytes needed to store root name of a key.
 *
 * Possible root key names are @p system or @p user .
 * This method does not consider the user domain in @p user:username keys.
 *
 * @param key the key object to work with
 * @return number of bytes needed with the ending NULL
 * @see keyGetFullRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetRootNameSize(const Key *key)
{
	if (!key->key) return 0;

	if (keyIsUser(key)) return sizeof("user");
	else return sizeof("system");
}



/*
 * Copy to @p returned the root name of @p key.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user
 * - root of @p user/env/env1 is @p user
 *
 * Use keyGetFullRootName() to get also the user domain.
 *
 * @param key the key to extract root from
 * @param returned a pre-allocated buffer to store the rootname
 * @param maxSize size of the @p returned buffer
 * @return number of bytes needed with ending NULL
 * @see keyGetRootNameSize(), keyGetFullRootName()
 * @ingroup keyname
 */
ssize_t keyGetRootName(const Key *key, char *returned, size_t maxSize)
{
	size_t size;

	if (!key->key) {
		/*errno=KDB_ERR_NOKEY;*/
		return -1;
	}

	if (!(size=keyGetRootNameSize(key))) {
		/*errno=KDB_ERR_NOKEY;*/
		return -1;
	}

	if (maxSize < size) {
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	} else strncpy(returned,key->key,size-1);
	returned[size-1]=0; /* null terminate it */
	return size;
}






/*
 * Gets number of bytes needed to store root name of a key name without
 * user domain.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user
 * - root of @p user/env/env1 is @p user
 *
 * @param keyName the name of the key
 * @return number of bytes needed with ending NULL or -1 if @p keyName is
 * 	not a valid name and @c errno is set to KDBErr::KDB_ERR_INVALIDKEY
 * @see keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetRootNameSize(const char *name)
{
	int length=strlen(name);

	if (!length) return 0;

	/*
		Possible situations:
	user
	user/
	user/blabla
	user:someuser
	user:someuser/
	user:someuser/key/name
	system
	system/
	system/blabla
	(empty)
	*/
	
	if (keyNameIsUser(name)) return sizeof("user");
	else if (keyNameIsSystem(name)) return sizeof("system");
	else {
		/*errno=KDB_ERR_INVALIDKEY;*/
		return -1;
	}
}





/*
 * Calculates number of bytes needed to store full root name of a key.
 *
 * Possible root key names are @p system, @p user or @p user:someuser.
 * In contrast to keyGetRootNameSize(), this method considers the user
 * domain part, and you should prefer this one.
 *
 * @param key the key object to work with
 * @return number of bytes needed with ending NULL
 * @see keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullRootNameSize(const Key *key)
{
	size_t size=0;

	if (keyIsUser(key))
	{
		if (keyGetMeta(key, "owner")) size=keyGetValueSize(keyGetMeta(key, "owner"));
		
		return size+sizeof("user");
	} else {
		return keyNameGetRootNameSize(key->key);
	}
}


/*
 * Copy to @p returned the full root name of the key.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user:denise
 * - root of @p user/env/env1 is @p user:$USER
 *
 * This method is more robust then keyGetRootName()
 *
 * @param key the key to extract root from
 * @param returned a pre-allocated buffer to store the rootname
 * @param maxSize size of the @p returned buffer
 * @return number of bytes written to @p returned including ending NULL
 * @see keyGetFullRootNameSize(), keyGetRootName()
 * @ingroup keyname
 */
ssize_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize)
{
	size_t size;
	size_t rootSize;
	char *cursor;

	if (!key->key) {
		/*errno=KDB_ERR_NOKEY;*/
		return 0;
	}

	if (!(size=keyGetFullRootNameSize(key))) {
		/*errno=KDB_ERR_NOKEY;*/
		return 0;
	}

	if (maxSize < size) {
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}
	
	rootSize = keyGetRootNameSize(key)-1;
	strncpy(returned,key->key, rootSize); /* copy "user" or "system" */
	if (keyIsUser(key) && keyGetMeta(key, "owner"))
	{
		cursor = returned + rootSize;
		*cursor = ':'; cursor++;
		strncpy (cursor, keyValue(keyGetMeta(key, "owner")), size - rootSize - 1); /* -1 for : */
	} else {
		returned[rootSize]=0;
	}

	return size;
}






/*
 * Get the number of bytes needed to store this key's parent name without
 * user domain, and with the ending NULL.
 *
 * @param key the key object to work with
 * @see keyGetParentName() for example
 * @ingroup keyname
 */
ssize_t keyGetParentNameSize(const Key *key)
{
	char *parentNameEnd=0;
	char *p;
	size_t size;

	if (!key->key) {
		/*errno=KDB_ERR_NOKEY;*/
		return 0;
	}

	/*
		user   (size=0)
		user/parent/base       (size=sizeof("user/parent"))
		user/parent/base/      (size=sizeof("user/parent"))
		user/parent/base\/name (size=sizeof("user/parent"))
	*/

	/* initialize */
	p=key->key;
	size=0;
	
	/* iterate over level names */
	while (*(p=keyNameGetOneLevel(p+size,&size))) parentNameEnd=p;
	
	/* handle NULL or root key ("user" or "system") */
	if (!parentNameEnd || parentNameEnd==key->key) return 0;

	/* at this point, parentNameEnd points to the first char of the basename */
	/* example: it points to the 'b' of "user/key/basename" */
	
	/* the delta is the size we want */
	return parentNameEnd - key->key;
}



/*
 * Copy this key's parent name (without owner) into a pre-allocated buffer.
 *
 * @par Example:
 * @code
Key *key=keyNew("system/parent/base",KEY_SWITCH_END);
char *parentName;
size_t parentSize;

parentSize=keyGetParentNameSize(key);
parentName=malloc(parentSize);
keyGetParentName(key,parentName,parentSize);
// work with parentName
free (parentName);
 * @endcode
 * @see keyGetParentNameSize()
 * @param key the key object to work with
 * @param returnedParent pre-allocated buffer to copy parent name to
 * @param maxSize number of bytes pre-allocated
 * @return number of bytes copied including ending NULL
 * @ingroup keyname
 */
ssize_t keyGetParentName(const Key *key, char *returnedParent, size_t maxSize)
{
	size_t parentSize;

	parentSize=keyGetParentNameSize(key);

	if (parentSize > maxSize) {
		/*errno=KDB_ERR_TRUNC;*/
		return 0;
	} else strncpy(returnedParent,key->key,parentSize);

	returnedParent[parentSize-1]=0; /* ending NULL */
	
	return parentSize;
}




/*
 * Return the namespace of a key name.
 *
 * Currently valid namespaces are KeyNamespace::KEY_NS_SYSTEM and KeyNamespace::KEY_NS_USER.
 *
 * @param keyName the name to deduce the namespace from
 * @return KeyNamespace::KEY_NS_SYSTEM, KeyNamespace::KEY_NS_USER
 * @return 0 for no valid namespace found (key has no name)
 * @see keyGetNamespace(), keyIsUser(), keyIsSystem()
 * @see #KeyNamespace
 * @ingroup keytest
 *
 */
int keyNameGetNamespace(const char *name)
{
	if (keyNameIsSystem(name)) return KEY_NS_SYSTEM;
	if (keyNameIsUser(name)) return KEY_NS_USER;
	return 0;
}



/*
 * Return the namespace of a key
 *
 * Currently valid namespaces are KeyNamespace::KEY_NS_SYSTEM and KeyNamespace::KEY_NS_USER.
 *
 * @param key the key object to work with
 * @return 0
 * @see keyIsUser(), keyIsSystem()
 * @ingroup keytest
 *
 */
int keyGetNamespace(const Key *key)
{
	if (keyIsUser (key)) return KEY_NS_USER;
	else if (keyIsSystem (key)) return KEY_NS_SYSTEM;
	else return 0;
}


/*
 * Check whether a key name is under the @p system namespace or not
 *
 * @return 1 if string begins with @p system , 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsUser()
 * @ingroup keyname
 *
 */
int keyNameIsSystem(const char *name)
{
	if (!strncmp("system",name,sizeof("system")-1)) return 1;
	return 0;
}


/*
 * Check whether a key name is under the @p user namespace or not
 *
 * @return 1 if string begins with @p user, 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsSystem()
 * @ingroup keyname
 *
 */
int keyNameIsUser(const char *name)
{
	if (!strncmp("user",name,sizeof("user")-1)) return 1;
	return 0;
}


/*********************************************************************
 *                Data constructors (protected)                      *
 *********************************************************************/
int keyInit(Key *key)
{
	memset(key,0,sizeof(Key));

	return 0;
}

