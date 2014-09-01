 /***************************************************************************
                      keyname.c  -  Methods for Key manipulation
                             -------------------
    begin                : Fri Sep 26 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/**
 * @defgroup keyname Name Manipulation Methods
 * @ingroup key
 * @brief Methods to do various operations on Key names.
 *
 * To use them:
 * @code
#include <kdb.h>
* @endcode
 *
 * These functions make it easier for C programmers to work with key names.
 *
 *
 * @par Terminology of Key Names
 * - A *key name* (see keySetName() and keyName()) defines the
 *   place of a key within the key database.
 *   To be unique, it is always absolute and canonical.
 * - Key names are composed out of many *key name parts* split by a
 *   separator. These *key name parts* do not contain a unescaped
 *   separator.
 * - A *key base name* (see keySetBaseName() and keyAddBaseName()) is
 *   the last part of the key name.
 * - A namespace denotes the place the key comes from:
 *   - _user_ keys come from user's home directories
 *   - _system_ keys come from systems etc directories
 * - A *C-String* is a null terminated sequence of characters.
 *   So \\0 (null-character) must not occur within a C-String.
 *
 *
 * @note The rules are currently not formally specified and are subject
 * of change in the next major release.
 * So, always prefer:
 * - To use keySetName() and keyAddName() to get the canonified version of the keyname
 * - To use keySetBaseName() and keyAddBaseName() to get an escaped key
 *   name part.
 * - Not to escape or canonify with your own algorithms!
 * - To use keyUnescapedName() and keyBaseName() to have access to the
 *   key name without escape sequences (key name parts are null
 *   terminated)
 * - Not to unescape the strings yourself!
 *
 *
 * @par Syntax for Key Names
 * Key names and key name parts have following goals:
 * - The C-String passed to keySetName() and keyAddName() may be any
 *   C-String.
 * - The *key name parts* (e.g. keySetBaseName(), keyBaseName()) may
 *   be any C-String.
 * Escaping is needed to achieve both goals.
 *
 *
 * @par Semantics for Key Name Parts
 * - \% denotes an empty key name part.
 *
 *
 * @par Canonicalization for Key Names
 * - / (slash) is the separator between key name parts.
 * - // is shortened to /
 * - trailing / (slashes) are removed
 * - . (dot) and .. (dot-dot) is removed in an canonical key name, with
 *   following rules:
 *   - /./ is shortened to /
 *   - _/../ is shortened to _
 *
 *
 * @par Conventions for key names
 * - Key name parts starting with \# are array elements.
 *   Then only _ (underscore) followed by 0-9 is allowed.
 *   So we have the regular expression #[_]*[0-9]+ with the further
 *   limitation that the number of _ is defined by the number of
 *   digits-1.
 * - Key name parts starting with _ are reserved for special purposes
 *   (if you use this within a plugin you still have to make sure _ is
 *   escaped properly)
 * - Key name parts starting with @ are reserved for special purposes
 *   (if you use this within a plugin you still have to make sure @ is
 *   escaped properly)
 * - If any key name part starts with . (dot) it means the key is
 *   inactive, see keyIsInactive().
 *
 *
 * @par Escaping rules
 * - \\ (backslash) is the escape character for the situations as
 *   described here (and only these).
 *   The \\ character must only be escaped, when one of the following
 *   rules apply. So there is no stray escape character possible.
 * - \\/ allows to escape /
 * - \\\\/ allows to use \\ as character before / (and so on)
 * - Use \\. and \\.. if you want your key name part to represent . and ..
 * - \\\\. and \\\\.. allows to use \\ as character before . and .. (and so on)
 * - Use \\% if you want your key name part to start with \% (and does
 *   not represent an empty name)
 * - Use \\\\% allows to use \\ as character before \% (and so on)

 *
 *
 * @par Semantics for Key Name Specifications
 * - _ denotes that the key name part is
 *   arbitrary (syntax as described above).
 * - \# denotes that the key name part
 *   has array syntax.
 * - names surrounded by \% (e.g. \%profile\%)
 *   denotes a placeholder.
 *
 *
 * @par Usage of Key Names
 * When using Elektra to store your application's configuration and state,
 * please keep in mind the following rules:
 * - Avoid to have your applications root right under @p system or @p user.
 *   (rationale: it would make the hierarchy too flat.)
 * - Avoid the usage of characters other then a-z, 0-9 and _.
 *   (rationale: it would allow too many similar, confusing names.)
 *   (exceptions: if the user or a technology, decide about parts of
 *   the key name, this restriction does not apply, e.g. if the wlan
 *   essid is used as part of the key name)
 * - It is suggested to make your application look for default keys under
 *   @p /sw/myapp/#/%/ where \# is a major version number, e.g. \#3 for
 *   the 4th version and % is a profile (% for default profile). This way, from
 *   a sysadmin perspective, it will be possible to copy the
 *   @p system/sw/myapp/#3/%/ tree to something like
 *   @p system/sw/myapp/#3/old/ and keep system clean and organized.
 *   Additionally, it is possible to start the old version of the app,
 *   using @p /sw/myapp/#2.
 *
 */




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
#include "kdbinternal.h"
#include "kdbhelper.h"



/*******************************************
 *    General name manipulation methods    *
 *******************************************/


/**
 * Returns a pointer to the abbreviated real internal @p key name.
 *
 * This is a much more efficient version of keyGetName() and can use
 * it if you are responsible enough to not mess up things. You are not allowed
 * to change anything in the returned array. The content of that string
 * may change after keySetName() and similar functions. If you need a copy of the name,
 * consider using keyGetName().
 *
 * The name will be without owner, see keyGetFullName() if
 * you need the name with its owner.
 *
 * keyName() returns "" when there is no keyName. The reason is
 * @code
key=keyNew(0);
keySetName(key,"");
keyName(key); // you would expect "" here
keyDel(key);
 * @endcode
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyName() method to set a new
 * value. Use keySetName() instead.
 *
 * @param key the key object to work with
 * @return a pointer to the keyname which must not be changed.
 * @return "" when there is no (a empty) keyname
 * @return 0 on NULL pointer
 * @see keyGetNameSize() for the string length
 * @see keyGetFullName(), keyGetFullNameSize() to get the full name
 * @see keyGetName() as alternative to get a copy
 * @see keyOwner() to get a pointer to owner
 * @ingroup keyname
 */
const char *keyName(const Key *key)
{
	if (!key) return 0;

	if (!key->key) {
		return "";
	}

	return key->key;
}

const void *keyUnescapedName(const Key *key)
{
	if (!key) return 0;

	if (!key->key) {
		return "";
	}

	return key->key+key->keySize;
}


/**
 * Bytes needed to store the key name without owner.
 *
 * For an empty key name you need one byte to store the ending NULL.
 * For that reason 1 is returned.
 *
 * @param key the key object to work with
 * @return number of bytes needed, including ending NULL, to store key name
 * 	without owner
 * @return 1 if there is is no key Name
 * @return -1 on NULL pointer
 * @see keyGetName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetNameSize(const Key *key)
{
	if (!key) return -1;

	if (!key->key)
	{
		return 1;
	}
	else return key->keySize;
}


ssize_t keyGetUnescapedNameSize(const Key *key)
{
	if (!key) return -1;

	if (!key->key)
	{
		return 0;
	}
	else return key->keyUSize;
}




/**
 * Get abbreviated key name (without owner name).
 *
 * When there is not enough space to write the name,
 * nothing will be written and -1 will be returned.
 *
 * maxSize is limited to SSIZE_MAX. When this value
 * is exceeded -1 will be returned. The reason for that
 * is that any value higher is just a negative return
 * value passed by accident. Of course malloc is not
 * as failure tolerant and will try to allocate.
 *
 * @code
char *getBack = malloc (keyGetNameSize(key));
keyGetName(key, getBack, keyGetNameSize(key));
 * @endcode
 *
 * @return number of bytes written to @p returnedName
 * @return 1 when only a null was written
 * @return -1 when keyname is longer then maxSize or 0 or any NULL pointer
 * @param key the key object to work with
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @see keyGetNameSize(), keyGetFullName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetName(const Key *key, char *returnedName, size_t maxSize)
{
	if (!key) return -1;

	if (!returnedName) return -1;

	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!key->key) {
		/*errno=KDB_ERR_NOKEY;*/
		returnedName[0]=0;
		return 1;
	}

	if (key->keySize > maxSize) {
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}

	strncpy(returnedName,key->key,maxSize);

	return key->keySize;
}

/**
 * @internal
 *
 * @brief Call this function after every key changing operation
 *
 * @pre key->key and key->keySize are set accordingly and the size of
 * allocation is twice as what you actually needed.
 *
 * @post we get a unsynced key with a correctly terminated
 * key name suitable for ordering and the name getter methods
 *
 * It will duplicate the key length and put a second name afterwards
 * that is used for sorting keys.
 *
 * @param key
 */
ssize_t elektraFinalizeName(Key *key)
{
	key->key[key->keySize - 1] = 0; /* finalize string */

	key->keyUSize = elektraUnescapeKeyName(key->key,
			key->key+key->keySize);

	key->flags |= KEY_FLAG_SYNC;

	return key->keySize;
}

ssize_t elektraFinalizeEmptyName(Key *key)
{
	key->key = elektraCalloc(2); // two null pointers
	key->keySize = 1;
	key->keyUSize = 1;
	key->flags |= KEY_FLAG_SYNC;

	return key->keySize;
}


/**
 * Set a new name to a key.
 *
 * A valid name is of the forms:
 * - @p system/something
 * - @p user/something
 * - @p user:username/something
 *
 * The last form has explicitly set the owner, to let the library
 * know in which user folder to save the key. A owner is a user name.
 * If it is not defined (the second form) current user is used.
 *
 * You should always follow the guidelines for key tree structure creation.
 *
 * A private copy of the key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * .., . and / will be handled as in filesystem pathes. A valid name will be build
 * out of the (valid) name what you pass, e.g. user///sw/../sw//././MyApp -> user/sw/MyApp
 *
 * On invalid names, NULL or "" the name will be "" afterwards.
 *
 *
 * @retval size in bytes of this new key name including ending NULL
 * @retval 0 if newName is an empty string or a NULL pointer (name will be empty afterwards)
 * @retval -1 if newName is invalid (name will be empty afterwards)
 * @retval -1 if key was inserted to a keyset before
 * @param key the key object to work with
 * @param newName the new key name
 * @see keyNew(), keySetOwner()
 * @see keyGetName(), keyGetFullName(), keyName()
 * @see keySetBaseName(), keyAddBaseName() to manipulate a name
 * @ingroup keyname
 */
ssize_t keySetName(Key *key, const char *newName)
{
	return elektraKeySetName(key, newName, 0);
}

ssize_t elektraKeySetName(Key *key, const char *newName,
		enum elektra_name_options options)
{
	size_t length;
	size_t rootLength, userLength, systemLength, ownerLength;
	char *p=0;

	if (!key) return -1;
	if (test_bit(key->flags,  KEY_FLAG_RO_NAME)) return -1;

	if (key->key) elektraFree(key->key);
	key->key = 0;
	key->keySize=1; /* equal to length plus room for \\0 */

	/* handle null new key name, removing the old name */
	if (!newName || !(length=elektraStrLen(newName)-1))
	{
		elektraFinalizeEmptyName(key);
		return 0; // we need to return 0 because of specification
	}

	rootLength=keyNameGetFullRootNameSize(newName)-1;
	if (!(options & KDB_O_CASCADING_NAME) && !rootLength)
	{
		return -1;
	}
	userLength=sizeof("user")-1;
	systemLength=sizeof("system")-1;
	ownerLength=rootLength-userLength;
	
	if (ownerLength>0) --ownerLength;

	if ((options & KDB_O_EMPTY_NAME) &&
		(!strcmp(newName, "")))
	{
		return elektraFinalizeEmptyName(key);
	}
	if ((options & KDB_O_CASCADING_NAME) &&
		(newName[0] == '/'))
	{
		if (!strcmp(newName, "/"))
		{
			key->key = elektraCalloc(4);
			key->key[0] = '/';
			key->keySize=2;
			elektraFinalizeName(key);
			return key->keySize;
		}
		/* handle cascading key names */
		rootLength = 1;
	}
	else if (options & KDB_O_META_NAME)
	{
		size_t size = 0;
		p=keyNameGetOneLevel(newName,&size);
		rootLength = size+1;
	}
	else if (keyNameIsUser(newName))
	{
		/* handle "user*" */
		if (length > userLength)
		{
			/* handle "user?*" */
			if (*(newName+userLength)==':')
			{
				/* handle "user:*" */
				if (ownerLength > 0)
				{
					char *owner;
					p=elektraMalloc(ownerLength+1);
					if (NULL==p) goto error_mem;
					owner=p;
					strncpy(owner,newName+userLength+1,ownerLength);
					owner[ownerLength]=0;
					keySetOwner(key, owner);
					elektraFree (owner);
				}
				key->keySize+=length-ownerLength-1;  /* -1 is for the ':' */
			} else if (*(newName+userLength)!=KDB_PATH_SEPARATOR) {
				/* handle when != "user/ *" */
				return -1;
			} else {
				/* handle regular "user/ *" */
				key->keySize+=length;
			}
		} else {
			/* handle "user" */
			key->keySize+=userLength;
		}

		rootLength  = userLength+1;
	}
	else if (keyNameIsSystem(newName))
	{
		/* handle "system*" */
		if (length > systemLength && *(newName+systemLength)!=KDB_PATH_SEPARATOR)
		{	/* handle when != "system/ *" */
			return -1;
		}
		key->keySize+=length;

		keySetOwner (key, NULL);

		rootLength  = systemLength+1;
	}
	else
	{
		/**Unsupported key name */
		return -1;
	}

	/*
	   At this point:
	   - key->key has no memory (re)allocated yet
	   - key->keySize has number of bytes that will be allocated for key name
	     with already removed owner. (even though we do not need it)
	   - owner is already set
	   - rootLength is sizeof("user") or sizeof("system")
	*/

	/* Allocate memory for key->key */
	p=elektraCalloc(rootLength);
	if (NULL==p) goto error_mem;
	key->key=p;

	/* copy the root of newName to final destination */
	strncpy(key->key,newName,rootLength);

	/* finish root name for keyAddName() */
	key->keySize=rootLength;
	key->key[rootLength-1] = '\0';

	size_t size = 0;
	if ((options & KDB_O_CASCADING_NAME) &&
		(newName[0] == '/'))
	{
		p = (char*)newName;
		size=1;
	}
	else
	{
		/* skip namespace we already processed */
		p=keyNameGetOneLevel(newName,&size);
	}

	return keyAddName(key, p+size);

error_mem:
	return -1;
}





/**
 * Bytes needed to store the key name including user domain and ending NULL.
 *
 * @param key the key object to work with
 * @return number of bytes needed to store key name including user domain
 * @return 1 on empty name
 * @return -1 on NULL pointer
 * @see keyGetFullName(), keyGetNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullNameSize(const Key *key)
{
	size_t returnedSize=0;

	if (!key) return -1;

	if (!key->key) return 1;

	returnedSize=elektraStrLen(key->key);

	if (keyNameIsUser(key->key) && keyGetMeta(key, "owner"))
		returnedSize+=keyGetOwnerSize(key);

	/*
	   After 2 elektraStrLen() calls looks like we counted one more NULL.
	   But we need this byte count because a full key name has an
	   additional ':' char.
	*/

	return returnedSize;
}




/**
 * Get key full name, including the user domain name.
 *
 * @return number of bytes written
 * @return 1 on empty name
 * @return -1 on NULL pointers
 * @return -1 if maxSize is 0 or larger than SSIZE_MAX
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @ingroup keyname
 */
ssize_t keyGetFullName(const Key *key, char *returnedName, size_t maxSize)
{
	size_t userSize=sizeof("user")-1;
	size_t ownerSize;
	ssize_t length;
	ssize_t maxSSize;
	char *cursor;

	if (!key) return -1;
	if (!returnedName) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;
	maxSSize = maxSize;

	length=keyGetFullNameSize(key);
	if (length == 1) {
		/*errno=KDB_ERR_NOKEY;*/
		returnedName[0]=0;
		return length;
	}
	else if (length < 0) return length;
	else if (length > maxSSize) {
		/* errno=KDB_ERR_TRUNC; */
		return -1;
	}

	cursor=returnedName;
	if (keyIsUser(key))
	{
		strncpy(cursor,key->key,userSize);
		cursor+=userSize;
		if (keyGetMeta(key, "owner"))
		{
			*cursor=':'; ++cursor;
			ownerSize=keyGetValueSize(keyGetMeta(key, "owner"))-1;
			strncpy(cursor,keyValue(keyGetMeta(key, "owner")),ownerSize);
			cursor+=ownerSize;
		}
		strcpy(cursor,key->key+userSize);
	} else strcpy(cursor,key->key);

	return length;
}



/**
 * @brief Returns a pointer to the internal unescaped key name where the @p basename starts.
 *
 * This is a much more efficient version of keyGetBaseName() and you should
 * use it if you are responsible enough to not mess up things. The name might
 * change or even point to a wrong place after a keySetName(). So make
 * sure to copy the memory before the name changes.
 *
 * keyBaseName() returns "" when there is no keyBaseName. The reason is
 * @snippet testabi_key.c base0 empty
 *
 * And there is also support for really empty basenames:
 * @snippet testabi_key.c base1 empty
 *
 * @note You must never use the pointer returned by keyBaseName()
 * method to change the name, but you should use keySetBaseName()
 * instead.
 *
 * @param key the object to obtain the basename from
 * @return a pointer to the basename
 * @return "" when the key has no (base)name
 * @return 0 on NULL pointer
 * @see keyGetBaseName(), keyGetBaseNameSize()
 * @see keyName() to get a pointer to the name
 * @see keyOwner() to get a pointer to the owner
 * @ingroup keyname
 */
const char *keyBaseName(const Key *key)
{
	if (!key) return 0;
	if (!key->key) return "";

	char *p = key->key + key->keySize + key->keyUSize - 1;

	char *base=p;
	while (*(--p)) base=p;

	if (base != (key->key + key->keyUSize)) return base;
	else return "";
}



/**
 * Calculates number of bytes needed to store basename of @p key.
 *
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 1 bytes to store "".
 *
 * Basenames are denoted as:
 * - @c system/some/thing/basename -> @c basename
 * - @c user:domain/some/thing/base\/name > @c base\/name
 *
 * @param key the key object to work with
 * @return size in bytes of @p key's basename including ending NULL
 * @see keyBaseName(), keyGetBaseName()
 * @see keyName(), keyGetName(), keySetName()
 * @ingroup keyname
 */
ssize_t keyGetBaseNameSize(const Key *key)
{
	const char * baseName = keyBaseName(key);
	if (!baseName) return -1;

	return elektraStrLen(baseName);
}



/**
 * Calculate the basename of a key name and put it in @p returned finalizing
 * the string with NULL.
 *
 * Some examples:
 * - basename of @c system/some/keyname is @c keyname
 * - basename of @c "user/tmp/some key" is @c "some key"
 *
 * @param key the key to extract basename from
 * @param returned a pre-allocated buffer to store the basename
 * @param maxSize size of the @p returned buffer
 * @return number of bytes copied to @p returned
 * @return 1 on empty name
 * @return -1 on NULL pointers
 * @return -1 when maxSize is 0 or larger than SSIZE_MAX
 * @see keyBaseName(), keyGetBaseNameSize()
 * @see keyName(), keyGetName(), keySetName()
 * @ingroup keyname
 */
ssize_t keyGetBaseName(const Key *key, char *returned, size_t maxSize)
{
	if (!key) return -1;
	if (!returned) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;
	ssize_t maxSSize = maxSize;

	if (!key->key)
	{
		returned[0] = 0;
		return 1;
	}

	ssize_t baseSize = keyGetBaseNameSize(key);
	if (maxSSize < baseSize)
	{
		return -1;
	}

	const char *baseName = keyBaseName(key);

	if (!baseName)
	{
		return -1;
	}

	strncpy(returned,baseName,baseSize);
	return baseSize;
}




/**
 * Adds @p baseName (that will be escaped) to the current key name.
 *
 * A new baseName will be added, no other part of the key name will be
 * affected.
 *
 * Assumes that @p key is a directory and will append @p baseName to it.
 * The function adds the path separator for concatenating.
 *
 * So if @p key has name @c "system/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have the name
 * @c "system/dir1/dir2/mykey".
 *
 * When @p baseName is 0 nothing will happen and the size of the name is returned.
 *
 * The escaping rules apply as in @link keyname above @endlink.
 *
 * A simple example is:
 * @snippet basename.c add base basic
 *
 * E.g. if you add . it will be escaped:
 * @snippet testabi_key.c base1 add
 *
 * @see keySetBaseName() to set a base name
 * @see keySetName() to set a new name.
 *
 * @param key the key object to work with
 * @param baseName the string to append to the name
 * @return the size in bytes of the new key name including the ending NULL
 * @return -1 if the key had no name
 * @return -1 on NULL pointers
 * @retval -1 if key was inserted to a keyset before
 * @ingroup keyname
 *
 */
ssize_t keyAddBaseName(Key *key, const char *baseName)
{
	if (!key) return -1;
	if (!baseName) return key->keySize;
	if (test_bit(key->flags,  KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	size_t size=0;
	char *escaped = elektraMalloc (strlen (baseName) * 2 + 2);
	elektraEscapeKeyNamePart(baseName, escaped);
	size = strlen (escaped);
	key->keySize += size + 1;
	elektraRealloc ((void**)&key->key, key->keySize*2);
	if (!key->key)
	{
		elektraFree (escaped);
		return -1;
	}

	key->key[key->keySize - size - 2] = KDB_PATH_SEPARATOR;
	memcpy (key->key + key->keySize - size - 1, escaped, size);

	elektraFree (escaped);

	elektraFinalizeName(key);

	return key->keySize;
}

/**
 * @brief Add a already escaped name to the keyname.
 *
 * The same way as in keySetName() this method finds the canonical pathname.
 * Unlike, keySetName() it adds it to an already existing name.
 *
 * @param key the key where a name should be added
 * @param newName the new name to append
 *
 * @retval -1 if key is a null pointer or did not have a valid name before
 * @retval -1 if newName is a null pointer or not a valid name (contains \\ in beginning)
 * @retval -1 on allocation errors
 * @retval -1 if key was inserted to a keyset before
 * @retval size of the new key
 */
ssize_t keyAddName(Key *key, const char *newName)
{
	if (!key) return -1;
	if (!newName) return key->keySize;
	if (test_bit(key->flags,  KEY_FLAG_RO_NAME)) return -1;
	// if (!elektraValidateKeyNamePart(newName)) return -1;
	if (!key->key) return -1;

	size_t const newSize = key->keySize + elektraStrLen(newName);
	size_t const rootLength = key->keySize;
	elektraRealloc ((void**)&key->key, newSize*2);
	if (!key->key) return -1;

	size_t size=0;
	const char * p = newName;

	-- key->keySize; // fix keySize for loop below

	/* iterate over each single folder name removing repeated '/', .  and .. */
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		// printf ("level: %s, size: %d\n", p, size);
		if (size == 1 && strncmp (p, ".",1) == 0)
		{
			/* printf ("ignore .\n"); */
			continue; /* just ignore current directory */
		}
		else if (size == 2 && strncmp (p, "..",2) == 0) /* give away directory */
		{
			key->key[key->keySize] = 0; /* initialize first (valgrind) */
			while (key->keySize >= rootLength && key->key[key->keySize] != KDB_PATH_SEPARATOR) key->keySize--;
			/* printf ("do .. (key->keySize: %d), key->key: %s, rootLength: %d, key->keySize: %d\n",
					key->keySize, key->key, rootLength, key->keySize); */
			continue;
		}
		/* Add a '/' to the end of key name */
		key->key[key->keySize]=KDB_PATH_SEPARATOR;
		key->keySize++;
		
		/* carefully append basenames */
		memcpy(key->key+key->keySize,p,size);
		key->keySize+=size;
	}

	/* remove unescaped trailing slashes */
	while (key->key[key->keySize-1] == KDB_PATH_SEPARATOR && key->key[key->keySize-2] != '\\') key->keySize--;
	key->keySize ++; /*for \\0 ending*/

	elektraFinalizeName(key);

	return key->keySize;
}




/**
 * Sets @c baseName as the new basename for @c key.
 *
 * Only the baseName will be affected and no other part of the key.
 *
 * All text after the last @c '/' in the @p key keyname is erased and
 * @p baseName is appended.
 *
 * So let us suppose @p key has name @c "system/dir1/dir2/key1". If @p baseName
 * is @c "key2", the resulting key name will be @c "system/dir1/dir2/key2".
 * If @p baseName is empty or NULL, the resulting key name will
 * be @c "system/dir1/dir2".
 *
 * This function does proper escaping on the supplied name argument.
 *
 * You can use all names to set as basename (e.g. . (dot), ..
 * (dot-dot), % and "" (empty)). They will be properly escaped.
 *
 * A simple example is:
 * @snippet basename.c set base basic
 *
 * If you do not want escaping, use keySetBaseName() instead. E.g. if
 * you want to add an inactive key, use:
 * @snippet testabi_key.c base1
 *
 * or when you want to add an array item, use:
 * @snippet testabi_key.c base2
 *
 * @see keyname for more details on special names
 *
 * @param key the key object to work with
 * @param baseName the string used to overwrite the basename of the key
 * @return the size in bytes of the new key name
 * @return -1 on NULL pointers
 * @retval -1 if key was inserted to a keyset before
 * @see keyAddBaseName()
 * @see keySetName() to set a new name
 * @ingroup keyname
 */
ssize_t keySetBaseName(Key *key, const char *baseName)
{
	if (!key) return -1;
	if (test_bit(key->flags,  KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	size_t size=0;
	char *searchBaseName=0;
	size_t searchBaseSize=0;
	char *p = key->key;

	while (*(p=keyNameGetOneLevel(p+size,&size)))
	{
		searchBaseName=p;
		searchBaseSize=size+1;
	}

	if (!searchBaseName || searchBaseName==key->key)
	{
		return -1;
	}

	// truncate the key
	key->keySize -= searchBaseSize;

	if (!baseName)
	{
		// just remove base name
		elektraFinalizeName(key);
		return key->keySize;
	}

	char *escaped = elektraMalloc (strlen (baseName) * 2 + 2);
	elektraEscapeKeyNamePart(baseName, escaped);
	size_t sizeEscaped = elektraStrLen (escaped);

	elektraRealloc((void**)&key->key, (key->keySize+sizeEscaped)*2);
	if (!key->key)
	{
		elektraFree (escaped);
		return -1;
	}

	key->key [key->keySize - 1] = KDB_PATH_SEPARATOR;
	memcpy (key->key + key->keySize,
			escaped, sizeEscaped);

	elektraFree (escaped);

	key->keySize += sizeEscaped;
	elektraFinalizeName(key);

	return key->keySize;
}



/*****************************************************
 *         General owner manipulation methods        *
 *****************************************************/




/**
 * Return a pointer to the real internal @p key owner.
 *
 * This is a much more efficient version of keyGetOwner() and you
 * should use it if you are responsible enough to not mess up things.
 * You are not allowed to modify the returned string in any way.
 * If you need a copy of the string, consider to use keyGetOwner() instead.
 *
 * keyOwner() returns "" when there is no keyOwner. The reason is
 * @code
key=keyNew(0);
keySetOwner(key,"");
keyOwner(key); // you would expect "" here
keySetOwner(key,"system");
keyOwner(key); // you would expect "" here
 * @endcode
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyOwner() method to set a new
 * value. Use keySetOwner() instead.
 *
 * @param key the key object to work with
 * @return a pointer to internal owner
 * @retval "" when there is no (a empty) owner
 * @retval 0 iff key is a NULL pointer
 * @see keyGetOwnerSize() for the size of the string with concluding 0
 * @see keyGetOwner(), keySetOwner()
 * @see keyName() for name without owner
 * @see keyGetFullName() for name with owner
 * @ingroup keyname
 */
const char *keyOwner(const Key *key)
{
	const char *owner;

	if (!key) return 0;
	owner = keyValue(keyGetMeta(key, "owner"));

	if (!owner)
	{
		return "";
	}

	return owner;
}






/**
 * Return the size of the owner of the Key with concluding 0.
 *
 * The returned number can be used to allocate a string.
 * 1 will returned on an empty owner to store the concluding 0
 * on using keyGetOwner().
 *
 * @code
char * buffer;
buffer = malloc (keyGetOwnerSize (key));
// use buffer and keyGetOwnerSize (key) for maxSize
 * @endcode
 *
 * @note that -1 might be returned on null pointer, so when you
 * directly allocate afterwards its best to check if you will pass
 * a null pointer before.
 *
 * @param key the key object to work with
 * @return number of bytes
 * @return 1 if there is no owner
 * @return -1 on NULL pointer
 * @see keyGetOwner()
 * @ingroup keyname
 */
ssize_t keyGetOwnerSize(const Key *key)
{
	ssize_t size;
	if (!key) return -1;

	size = keyGetValueSize(keyGetMeta (key, "owner"));

	if (!size || size == -1)
	{
		/*errno=KDB_ERR_NODESC;*/
		return 1;
	}

	return size;
}



/**
 * Return the owner of the key.
 * - Given @p user:someuser/..... return @p someuser
 * - Given @p user:some.user/.... return @p some.user
 * - Given @p user/.... return the current user
 *
 * Only @p user/... keys have a owner.
 * For @p system/... keys (that doesn't have a key owner) an empty
 * string ("") is returned.
 *
 * Although usually the same, the owner of a key is not related to its
 * UID. Owner are related to WHERE the key is stored on disk, while
 * UIDs are related to mode controls of a key.
 *
 * @param key the object to work with
 * @param returnedOwner a pre-allocated space to store the owner
 * @param maxSize maximum number of bytes that fit returned
 * @return number of bytes written to buffer
 * @return 1 if there is no owner
 * @return -1 on NULL pointers
 * @return -1 when maxSize is 0, larger than SSIZE_MAX or too small for ownername
 * @see keySetName(), keySetOwner(), keyOwner(), keyGetFullName()
 * @ingroup keyname
 */
ssize_t keyGetOwner(const Key *key, char *returnedOwner, size_t maxSize)
{
	const char *owner;
	size_t ownerSize;
	if (!key) return -1;

	if (!maxSize) return -1;
	if (!returnedOwner) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	owner = keyValue(keyGetMeta(key, "owner"));
	ownerSize = keyGetValueSize(keyGetMeta(key, "owner"));

	if (!owner)
	{
		/*errno=KDB_ERR_NODESC;*/
		returnedOwner[0]=0;
		return 1;
	}

	strncpy(returnedOwner,owner,maxSize);
	if (maxSize < ownerSize) {
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}
	return ownerSize;
}



/**
 * Set the owner of a key.
 *
 * A owner is a name of a system user related to a UID.
 * The owner decides on which location on the disc the key
 * goes.
 *
 * A private copy is stored, so the passed parameter can be freed after
 * the call.
 *
 * @param key the key object to work with
 * @param newOwner the string which describes the owner of the key
 * @return the number of bytes actually saved including final NULL
 * @return 1 when owner is freed (by setting 0 or "")
 * @return -1 on null pointer or memory problems
 * @see keySetName(), keyGetOwner(), keyGetFullName()
 * @ingroup keyname
 */
ssize_t keySetOwner(Key *key, const char *newOwner)
{
	if (!key) return -1;
	if (!newOwner || *newOwner==0)
	{
		keySetMeta (key, "owner", 0);
		return 1;
	}

	keySetMeta (key, "owner", newOwner);
	return keyGetOwnerSize (key);
}
