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

#if DEBUG && defined(HAVE_STDIO_H)
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



/**
 * @internal
 *
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
		++real;
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




/**
 * @internal
 *
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





/**
 * @internal
 *
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



/**
 * @internal
 *
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


int keyNameIsSpec(const char *name)
{
	if (!strcmp("spec", name) ||
		!strncmp("spec/",name,sizeof("spec/")-1)) return 1;
	return 0;
}

int keyNameIsProc(const char *name)
{
	if (!strcmp("proc", name) ||
		!strncmp("proc/",name,sizeof("proc/")-1)) return 1;
	return 0;
}

int keyNameIsDir(const char *name)
{
	if (!strcmp("dir", name) ||
		!strncmp("dir/",name,sizeof("dir/")-1)) return 1;
	return 0;
}

/**
 * @internal
 *
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
	if (!strcmp("user", name) ||
		!strncmp("user/",name,sizeof("user/")-1) ||
		!strncmp("user:",name,sizeof("user:")-1)  ) return 1;
	return 0;
}

/**
 * @internal
 *
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
	if (!strcmp("system", name) ||
		!strncmp("system/",name,sizeof("system/")-1)) return 1;
	return 0;
}


/**
 * @internal
 *
 * clears key (all data members are set to zero)
 */
int keyInit(Key *key)
{
	memset(key,0,sizeof(Key));

	return 0;
}

/**
 * @internal
 *
 * helper functions for keyNew/keyVNew
 *
 * @pre caller must use va_start and va_end on va
 * @param key initialized Key
 * @param keyName a valid name to the key, or NULL to get a simple
 * initialized, but really empty, object
 * @param va the variadic argument list
 */
void keyVInit (Key *key, const char *name, va_list va)
{
	keyswitch_t action = 0;
	void *value = 0;
	int valueSizeChanged = 0;
	size_t valueSize = 0;
	char *owner = 0;
	option_t nameOptions = 0;
	void (*p) (void) = 0;

	if (!key) return;

	if (name) {
		action=va_arg(va, keyswitch_t);
		while (action) {
			switch (action) {
				case KEY_SIZE:
					valueSize=va_arg(va, size_t);
					valueSizeChanged = 1;
					break;
				case KEY_BINARY:
					keySetMeta (key, "binary", "");
					break;
				case KEY_VALUE:
					value = va_arg(va, void *);
					if (valueSizeChanged && keyIsBinary(key))
					{
						keySetBinary(key,value, valueSize);
					} else if (keyIsBinary(key)) {
						valueSize = elektraStrLen (value);
						keySetBinary(key,value, valueSize);
					} else {
						keySetString(key,value);
					}
					break;
#ifndef WIN32
				case KEY_UID:
					keySetUID(key,va_arg(va,uid_t));
					break;
				case KEY_GID:
					keySetGID(key,va_arg(va,gid_t));
					break;
				case KEY_MODE:
					/* Theoretically this should be mode_t, but prefer using
					   int to avoid troubles when sizeof(mode_t)!=sizeof(int)
					 */
					keySetMode(key,va_arg(va, int));
					break;
				case KEY_DIR:
					keySetDir(key);
					break;
#endif
				case KEY_OWNER:
					owner = va_arg(va,char *);
					break;
				case KEY_COMMENT:
					keySetComment(key,va_arg(va,char *));
					break;
				case KEY_LOCK_NAME:
					keyLock(key, KEY_LOCK_NAME);
					break;
				case KEY_LOCK_VALUE:
					keyLock(key, KEY_LOCK_VALUE);
					break;
				case KEY_LOCK_META:
					keyLock(key, KEY_LOCK_META);
					break;
				case KEY_FUNC:
					p = va_arg(va, void(*)(void));
					keySetBinary(key, &p, sizeof(p));
					break;
				case KEY_META:
					value = va_arg (va,char *);
					/*First parameter is name*/
					keySetMeta (key, value, va_arg(va,char *));
					break;
				case KEY_CASCADING_NAME:
					nameOptions |= KEY_CASCADING_NAME;
					break;
				case KEY_META_NAME:
					nameOptions |= KEY_META_NAME;
					break;
				case KEY_EMPTY_NAME:
					/* actually useless in current
					 * implementation, empty name
					 * is ok anyway. Maybe if error
					 * handling is visible to user
					 * in future the option still
					 * might be interesting */
					nameOptions |= KEY_EMPTY_NAME;
					break;
				default:
#if DEBUG
					fprintf (stderr, "Unknown option in keyVInit %ld\n", (long int)action);
#endif
					break;
			}
			action=va_arg(va, keyswitch_t);
		}

		elektraKeySetName(key, name, nameOptions);
		if (owner) keySetOwner(key, owner);
	}
}

