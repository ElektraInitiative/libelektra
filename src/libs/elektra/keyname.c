/**
 * @file
 *
 * @brief Methods for Key name manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

/** @class doxygenNamespaces
 *
 * @brief .
 *
 * .
 *
 * - @p spec:/something for specification of other keys.
 * - @p proc:/something for in-memory keys, e.g. commandline.
 * - @p dir:/something for dir keys in current working directory
 * - @p system:/something for system keys in /etc or /
 * - @p user:/something for user keys in home directory
 * - @p user:username/something for other users (deprecated: kdbGet() + kdbSet() currently unsupported)
 * - @p /something for cascading keys (actually refers to one of the above, see also ksLookup())
 *
 */


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
 *   separator. These *key name parts* do not contain an unescaped
 *   separator.
 * - A *key base name* (see keySetBaseName() and keyAddBaseName()) is
 *   the last part of the key name.
 * - A *C-String* is a null terminated sequence of characters.
 *   So \\0 (null-character) must not occur within a C-String.
 *
 * @par Namespaces
 * A namespace denotes the place the key comes from:
 *
 * @copydetails doxygenNamespaces
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
 *
 *
 * @par Escaping rules
 * - \\ (backslash) is the escape character for the situations as
 *   described here (and only these).
 *   The \\ character must only be escaped, when one of the following
 *   rules apply.
 * - Stray escape characters are only possible in the end of the string.
 * - \\/ allows one to escape / (any uneven number of \\).
 *   Does not introduce a new part.
 * - Any uneven number N of \\ before / allows you to escape / with the
 *   N/2 of \\ prefixed.
 *   Does not introduce a new part.
 * - \\\\/ allows one to use \\ as character before / and introduces a new
 *   part.
 * - Any even number N of \\ before / allows you to have N/2 of \\
 *   prefixed before a / which introduces a new part.
 * - Use \\. and \\.. if you want your key name part to represent . and ..
 * - \\\\. and \\\\.. allows us to use \\ as character before . and .. (and so on)
 * - Use \\% if you want your key name part to start with \% (and does
 *   not represent an empty name)
 * - Using \\\\% allows one to use \\ as character before \% (and so on)

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
 * @{
 */


#include "kdbprivate.h"
#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbassert.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <ctype.h>

#include "kdb.h"
#include "kdbhelper.h"
#include "kdbinternal.h"

/**
 * Helper method: returns a pointer to the start of the last part of the given key name
 *
 * @param name a canonical key name
 * @param len the length of the key name
 *
 * @returns pointer to the start of the last part within @p name
 */
static char * findStartOfLastPart (char * name, size_t len)
{
	char * colon = strchr (name, ':');
	char * start = colon == NULL ? name : colon + 1;
	++start; // start after first slash

	char * cur = start + len - (start - name) - 1;

	if (cur == start) return NULL; // no base name

	while (cur >= start)
	{
		--cur;
		while (cur >= start && *cur != '/')
		{
			--cur;
		}

		size_t backslashes = 0;
		while (cur - backslashes > start && *(cur - backslashes - 1) == '\\')
		{
			++backslashes;
		}

		if (backslashes % 2 == 0)
		{
			break;
		}
	}

	return cur < start - 1 ? NULL : cur;
}


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
 *
 * @retval "" when there is no keyName. The reason is
 * @code
key=keyNew(0);
keySetName(key,"");
keyName(key); // you would expect "" here
keyDel(key);
 * @endcode
 *
 * Valid key names are:
 *
 * @copydetails doxygenNamespaces
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyName() method to set a new
 * value. Use keySetName() instead.
 *
 * @param key the Key you want to get the name from
 *
 * @return a pointer to the Key's name which must not be changed.
 * @retval "" when Key's name is empty
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetNameSize() for the string length
 * @see keyGetName() as alternative to get a copy
 * @see keyUnescapedName to get an unescaped key name
 */
const char * keyName (const Key * key)
{
	if (!key) return 0;

	ELEKTRA_ASSERT (key->key != NULL, "invalid name");
	return key->key;
}

/**
 * Bytes needed to store the Key's name (excluding owner).
 *
 * For an empty Key name you need one byte to store the ending NULL.
 * For that reason, 1 is returned when the name is empty.
 *
 * @param key the Key to get the name size from
 *
 * @return number of bytes needed, including NULL terminator, to store Key's name
 * 	(excluding owner)
 * @retval 1 if Key has no name
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetName() for getting the Key's name
 * @see keyGetUnescapedNameSize() for getting the size of the unescaped name
 */
ssize_t keyGetNameSize (const Key * key)
{
	if (!key) return -1;

	return key->keySize;
}


/**
 * Returns a Key's name, separated by NULL bytes and without backslashes for
 * escaping
 *
 * Slashes are replaced with NULL bytes.
 * Therefore unescaped names of cascading Keys start with a NULL byte.
 * Otherwise escaped characters, e.g. non-hierarchy slashes, will be unescaped.
 *
 * This name is essential if you want to iterate over parts of the Key's
 * name, compare Key names or check relations of Keys in the hierarchy.
 *
 * @param key the Key whose unescaped name to get
 *
 * @return the name in its unescaped form
 * @retval 0 on NULL pointers
 * @retval "" if Key's name is empty
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetUnescapedName() for getting a copy of the unescaped name
 * @see keyGetUnescapedNameSize() for getting the size of the unescaped name
 * @see keyName() for getting the escaped name of the Key
 */
const void * keyUnescapedName (const Key * key)
{
	if (!key) return 0;
	ELEKTRA_ASSERT (key->ukey != NULL, "invalid name");
	return key->ukey;
}


/**
 * @brief Returns the size of the Key's unescaped name including embedded and
 * terminating NULL characters
 *
 * @param key the Key where to get the size of the unescaped name from
 *
 * @return The size of the Key's unescaped name
 * @retval -1 on NULL pointer
 * @retval 0 if Key has no name
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyUnescapedName() for getting a pointer to the unescaped name
 * @see keyGetUnescapedName() for getting a copy of the unescaped name
 * @see keyGetNameSize() for getting the size of the escaped name
 */
ssize_t keyGetUnescapedNameSize (const Key * key)
{
	if (!key) return -1;

	return key->keyUSize;
}


/**
 * Get abbreviated Key name (excluding owner).
 *
 * When there is not enough space to write the name,
 * nothing will be written and -1 will be returned.
 *
 * @p maxSize is limited to SSIZE_MAX. When this value
 * is exceeded, -1 will be returned. The reason for that
 * is, that any value higher is just a negative return
 * value passed by accident. elektraMalloc() is not
 * as failure tolerant and would try to allocate memory
 * accordingly.
 *
 * @code
char *getBack = elektraMalloc (keyGetNameSize(key));
keyGetName(key, getBack, keyGetNameSize(key));
 * @endcode
 *
 * @param key the Key to get the name from
 * @param returnedName pre-allocated buffer to write the Key's name
 * @param maxSize maximum number of bytes that will fit in returnedName,
 * including the NULL terminator
 *
 * @return number of bytes written to @p returnedName
 * @retval 1 when only NULL terminator was written
 * @retval -1 when Key's name is longer than maxSize or maxSize is 0 or maxSize
 * is greater than SSIZE_MAX
 * @retval -1 @p key or @p returnedName is NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetNameSize() for getting the size of a Key's name
 * @see keyName() for getting a pointer to a Key's name
 * @see keyGetBaseName() for getting a Key's base name
 * @see keyGetNamespace() for getting the namespace of a Key's name
 */
ssize_t keyGetName (const Key * key, char * returnedName, size_t maxSize)
{
	if (!key) return -1;

	if (!returnedName) return -1;

	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!key->key)
	{
		returnedName[0] = 0;
		return 1;
	}

	if (key->keySize > maxSize)
	{
		return -1;
	}

	memcpy (returnedName, key->key, key->keySize);

	return key->keySize;
}

/**
 * Copies the unescaped name of a Key into @p returnedName.
 *
 * It will only copy the whole name. If the buffer is too small,
 * an error code will be returned.
 *
 * To ensure the buffer is big enough, you can use keyGetUnescapedNameSize()
 * to get the correct size.
 *
 * @param key          the Key to extract the unescaped name from
 * @param returnedName the buffer to write the unescaped name into
 * @param maxSize      maximum number of bytes that can be copied
 * into @p returnedName
 *
 * @pre @p key MUST be a valid #Key and `key != NULL`
 * @pre @p returnedName MUST be allocated to be at least @p maxSize bytes big
 * @pre @p returnedName must not be NULL
 *
 * @returns the actual size of the Key's unescaped name, i.e. the number of
 * bytes copied into @p returnedName
 * @retval -1 Precondition error
 * @retval -2 the size of the unescaped name is greater than @p maxSize
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetUnescapedNameSize() for getting the size of the unescaped name
 * @see keyGetName() for getting the Key's escaped name
 */
ssize_t keyGetUnescapedName (const Key * key, char * returnedName, size_t maxSize)
{
	if (!key) return -1;
	if (!returnedName) return -1;

	if (!key->ukey)
	{
		returnedName[0] = 0;
		return 1;
	}

	if (key->keyUSize > maxSize)
	{
		return -2;
	}

	memcpy (returnedName, key->ukey, maxSize);

	return key->keyUSize;
}

/**
 * Set a new name to a Key.
 *
 * A valid name is one of the forms:
 * @copydetails doxygenNamespaces
 *
 * An invalid name either has an invalid namespace or
 * a wrongly escaped \\ at the end of the name.
 *
 * See @link keyname key names @endlink for the exact rules.
 *
 * The last form has explicitly set the owner, to let the library
 * know in which user folder to save the Key. A owner is a user name.
 * If it is not defined (the second form), current user is used.
 *
 * You should always follow the guidelines for Key tree structure creation.
 *
 * A private copy of the Key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * .., . and / will be handled as in filesystem paths. A valid name will be build
 * out of the (valid) name what you pass, e.g. user:///sw/../sw//././MyApp -> user:/sw/MyApp
 *
 * Trailing slashes will be stripped.
 *
 * On invalid names, the name stays unchanged.
 *
 * @return size of the new Key name in bytes, including NULL terminator
 * @retval -1 if @p key or @p keyName is NULL or @p keyName is empty or invalid
 * @retval -1 if Key was inserted to a KeySet before
 * @retval -1 if Key name is read-only
 *
 * @param key the Key whose name to set
 * @param newName the new name for the Key
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetName() for getting a copy of the Key's name
 * @see keyName() for getting a pointer to the Key's name
 * @see keySetBaseName(), keyAddBaseName() for manipulating the base name
 */
ssize_t keySetName (Key * key, const char * newName)
{
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (newName == NULL || strlen (newName) == 0) return -1;

	if (!elektraKeyNameValidate (newName, true))
	{
		// error invalid name
		return -1;
	}

	// from now on this function CANNOT fail -> we may modify the key

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and set NULL to allow realloc
		key->key = NULL;
		key->keySize = 0;
		key->ukey = NULL;
		key->keyUSize = 0;
		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	elektraKeyNameCanonicalize (newName, &key->key, &key->keySize, 0, &key->keyUSize);

	elektraRealloc ((void **) &key->ukey, key->keyUSize);

	elektraKeyNameUnescape (key->key, key->ukey);

	set_bit (key->flags, KEY_FLAG_SYNC);

	return key->keySize;
}

/**
 * Add an already escaped name part to the Key's name.
 *
 * The same way as in keySetName() this method finds the canonical pathname:
 * - it will ignore /./
 * - it will remove a level when /../ is used
 * - it will remove multiple slashes ////
 *
 * For example:
 * @snippet keyName.c add name
 *
 * Unlike keySetName() it adds relative to the previous name and
 * cannot change the namespace of a Key.
 * For example:
 * @snippet keyName.c namespace
 *
 * The passed name needs to be valid according the @link keyname key name rules @endlink.
 * It is not allowed to:
 * - be empty
 * - end with unequal number of \\
 *
 * @pre @p key MUST be a valid #Key
 *
 * @param key the Key where a name should be added
 * @param newName the new name to add to the name of @p key
 *
 * @returns new size of the escaped name of @p key
 * @retval -1 if `key == NULL` or `newName == NULL`
 * @retval -1 @p newName is not a valid escaped name
 * @retval -1 @p key is read-only
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keySetName() for setting a Key's name
 * @see keyAddBaseName() for adding a basename to a Key
 */
ssize_t keyAddName (Key * key, const char * newName)
{
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!newName) return -1;

	while (*newName == '/')
	{
		// skip leading slashes
		++newName;
		if (*newName == '.' && *(newName + 1) == '/')
		{
			// also skip /./ parts
			newName += 2;
		}
	}

	if (strlen (newName) == 0) return key->keySize;

	if (!elektraKeyNameValidate (newName, false))
	{
		// error invalid name suffix
		return -1;
	}

	// from now on this function CANNOT fail -> we may modify the key

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and copy to malloced buffer
		char * tmp = elektraMalloc (key->keySize);
		memcpy (tmp, key->key, key->keySize);
		key->key = tmp;

		tmp = elektraMalloc (key->keyUSize);
		memcpy (tmp, key->ukey, key->keyUSize);
		key->ukey = tmp;

		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	elektraKeyNameCanonicalize (newName, &key->key, &key->keySize, key->keySize, &key->keyUSize);

	elektraRealloc ((void **) &key->ukey, key->keyUSize);

	elektraKeyNameUnescape (key->key, key->ukey);

	set_bit (key->flags, KEY_FLAG_SYNC);
	return key->keySize;
}

static size_t replacePrefix (char ** buffer, size_t size, size_t oldPrefixSize, const char * newPrefix, size_t newPrefixSize)
{
	size_t newSize;
	if (size == oldPrefixSize)
	{
		// overwrite everything
		newSize = newPrefixSize;
		elektraRealloc ((void **) buffer, newSize);
		memcpy (*buffer, newPrefix, newPrefixSize);
	}
	else if (oldPrefixSize < newPrefixSize)
	{
		// grow, move, overwrite
		newSize = size + (newPrefixSize - oldPrefixSize);
		elektraRealloc ((void **) buffer, newSize);
		memmove (*buffer + newPrefixSize, *buffer + oldPrefixSize, size - oldPrefixSize);
		memcpy (*buffer, newPrefix, newPrefixSize);
	}
	else
	{
		// move, overwrite, shrink
		newSize = size - (oldPrefixSize - newPrefixSize);
		memmove (*buffer + newPrefixSize, *buffer + oldPrefixSize, size - oldPrefixSize);
		memcpy (*buffer, newPrefix, newPrefixSize);
		elektraRealloc ((void **) buffer, newSize);
	}
	return newSize;
}

/**
 * Replaces a prefix of the key name of @p key.
 *
 * The function only modifies @p key, if is is below (or same as)
 * @p oldPrefix (see keyIsBelowOrSame()) and they both have the
 * same namespace (this is not always the case with keyIsBelowOrSame()).
 *
 * In simple terms this function operates as follows:
 * 1. If before calling this function @p key and @p oldPrefix had the
 *    same name, then afterwards @p key will have the same name as
 *    @p newPrefix.
 * 2. If @p key was in the same namespace as and below @p oldPrefix, then
 *    after calling this function @p key will be in the same namespace as
 *    and below @p newPrefix.
 * 3. Otherwise @p key will not be modified.
 *
 * Note: We use `const Key *` arguments for the prefixes instead of
 * `const char *` to ensure only valid key names can be passed as arguments.
 *
 * @param key       The key that will be manipulated.
 * @param oldPrefix The name of this key will be removed from the front
 *                  of the name of @p key.
 * @param newPrefix The name of this key will be added to the front of
 *                  @p key, after the name of @p oldPrefix is removed.
 *
 * @retval -1 if @p key, @p oldPrefix or @p newPrefix are NULL
 *            or the name of @p key is marked as read-only
 * @retval  0 if @p key is not below (or same as) @p oldPrefix,
 *            i.e. there is no prefix to replace
 * @retval  1 if the prefix was sucessfully replaced
 */
int keyReplacePrefix (Key * key, const Key * oldPrefix, const Key * newPrefix)
{
	if (key == NULL || oldPrefix == NULL || newPrefix == NULL) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;

	// check namespace manually, because keyIsBelowOrSame has special handling for cascading keys
	if (keyGetNamespace (key) != keyGetNamespace (oldPrefix)) return 0;
	if (keyIsBelowOrSame (oldPrefix, key) != 1) return 0;

	// same prefix -> nothing to do
	if (keyCmp (oldPrefix, newPrefix) == 0) return 1;

	if (key->keyUSize == oldPrefix->keyUSize)
	{
		// key is same as oldPrefix -> just copy name
		keyCopy (key, newPrefix, KEY_CP_NAME);
		return 1;
	}

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and copy to malloced buffer
		char * tmp = elektraMalloc (key->keySize);
		memcpy (tmp, key->key, key->keySize);
		key->key = tmp;

		tmp = elektraMalloc (key->keyUSize);
		memcpy (tmp, key->ukey, key->keyUSize);
		key->ukey = tmp;

		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	size_t oldSize, oldUSize;
	if (oldPrefix->keyUSize == 3)
	{
		// oldPrefix is root key -> needs special handling
		oldSize = oldPrefix->keySize - 2;
		oldUSize = 2;
	}
	else
	{
		oldSize = oldPrefix->keySize - 1;
		oldUSize = oldPrefix->keyUSize;
	}

	size_t newSize, newUSize;
	if (newPrefix->keyUSize == 3)
	{
		// newPrefix is root key -> needs special handling
		newSize = newPrefix->keySize - 2;
		newUSize = 2;
	}
	else
	{
		newSize = newPrefix->keySize - 1;
		newUSize = newPrefix->keyUSize;
	}

	key->keySize = replacePrefix (&key->key, key->keySize, oldSize, newPrefix->key, newSize);
	key->keyUSize = replacePrefix (&key->ukey, key->keyUSize, oldUSize, newPrefix->ukey, newUSize);

	return 1;
}

/**
 * Takes an escaped key name and validates it.
 * Complete key names must inlcude a namespace or a leading slash.
 *
 * @param name       The escaped key name to check
 * @param isComplete Whether or not @p name is supposed to be a complete key name
 *
 * @retval #true If @p name is a valid key name.
 * @retval #false Otherwise
 *
 * @ingroup keyname
 */
bool elektraKeyNameValidate (const char * name, bool isComplete)
{
	if (name == NULL || (strlen (name) == 0 && isComplete)) return 0;

	if (isComplete)
	{
		const char * colon = strchr (name, ':');
		if (colon != NULL)
		{
			if (elektraReadNamespace (name, colon - name) == KEY_NS_NONE)
			{
				ELEKTRA_LOG_DEBUG ("Illegal namespace '%.*s': %s", (int) (colon - name - 1), name, name);
				return 0;
			}

			if (*(colon + 1) != '/')
			{
				ELEKTRA_LOG_DEBUG ("Missing slash after namespace: %s", name);
				return 0;
			}

			name = colon + 1;
		}

		if (*name != '/')
		{
			ELEKTRA_LOG_DEBUG ("Illegal name start; expected (namespace +) slash: %s", name);
			return 0;
		}
	}

	const char * cur = name;
	while ((cur = strchr (cur, '\\')) != NULL)
	{
		++cur;
		switch (*cur)
		{
		case '\0':
			ELEKTRA_LOG_DEBUG ("Dangling escape: %s", name);
			return 0;
		case '\\':
		case '/':
			++cur;
			// allowed anywhere
			continue;
		case '%':
			if (*(cur - 2) == '/' && (*(cur + 1) == '/' || *(cur + 1) == '\0')) continue;

			break;
		case '.':
			if (*(cur - 2) == '/' && (*(cur + 1) == '/' || *(cur + 1) == '\0')) continue;
			if (*(cur - 2) == '/' && *(cur + 1) == '.' && (*(cur + 2) == '/' || *(cur + 2) == '\0')) continue;

			break;
		case '#': {
			const char * end = cur + 1;
			while (isdigit (*end))
			{
				++end;
			}
			size_t len = end - cur;

			bool check1 = *end == '/' || *end == '\0';
			bool check2 = len < 20 || strncmp (cur + 1, "9223372036854775807", 19) <= 0;
			bool check3 = *(cur + 1) != '0';

			if (check1 && check2 && check3) continue;

			break;
		}
		}


		ELEKTRA_LOG_DEBUG ("Illegal escape '\\%c': %s", *cur, name);
		return 0;
	}

	return 1;
}

/**
 * Takes a valid (non-)canonical key name and produces its canonical form.
 * As a side-effect it can also calculate the size of the corresponding unescaped key name.
 *
 * @param name          The key name that is processed
 * @param canonicalName Output buffer for the canonical name
 * @param canonicalSizePtr Pointer to size of @p canonicalName
 * @param offset        Offset into @p canonicalName
 * @param usizePtr      Output variable for the size of the unescaped name
 *
 * @pre @p name MUST be a valid (non-)canonical key name. If it is not, the result is undefined
 * @pre @p canonicalName MUST be a valid first argument for elektraRealloc() when cast to void**
 * @pre @p canonicalSizePtr >= @p offset
 * @pre @p offset MUST be 0 or `*canonicalName + offset` MUST point to the zero-termintor of a valid canonical key name that starts at
 * `*canonicalName`
 * @pre if @p offset is 0 then `*usizePtr` MUST 0, otherwise `*usizePtr` MUST be the correct unescaped size of the existing canonical name
 * in `*canonicalName`
 *
 * @see elektraKeyNameValidate
 *
 * @ingroup keyname
 */
void elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t * canonicalSizePtr, size_t offset, size_t * usizePtr)
{
	size_t nameLen = strlen (name) + 1;

	// ensure output is at least as big as input
	// output buffer will be enlarged when needed
	// at the end we shrink to the correct size
	if (offset + nameLen + 1 > *canonicalSizePtr)
	{
		*canonicalSizePtr = offset + nameLen;
		elektraRealloc ((void **) canonicalName, *canonicalSizePtr);
	}

	char * outPtr;
	size_t usize;

	// stores start of first part
	size_t rootOffset;
	if (offset == 0)
	{
		// namespace byte
		usize = 1;
		outPtr = *canonicalName;

		if (*name != '/')
		{
			// find end of namespace
			const char * colon = strchr (name, ':');

			// copy namespace
			memcpy (outPtr, name, colon - name + 1);
			outPtr += colon - name + 1;
			name = colon + 1;
		}

		rootOffset = outPtr - *canonicalName;

		// handle root slash
		*outPtr++ = '/';
		usize += 1;
	}
	else
	{
		usize = *usizePtr;
		outPtr = *canonicalName + offset - 2;

		if (usize == 3)
		{
			// root key -> re-use trailing slash
			--usize;
			++outPtr;
		}
		else
		{
			// not root key -> add trailing slash
			*++outPtr = '/';
			++outPtr;
		}

		// find root slash
		if (**canonicalName == '/')
		{
			rootOffset = 0;
		}
		else
		{
			rootOffset = strchr (*canonicalName, ':') - *canonicalName + 1;
		}
	}

	while (*name != '\0')
	{
		// STATE: name -- on part separator or first char of part
		//        outPtr -- start of new part
		//        usize -- namespace + separator + previous parts + separator
		//     -> skip part separator
		if (*name == '/')
		{
			++name;
		}

		// STATE: name -- first char of part
		//        outPtr -- start of new part
		//        usize -- namespace + separator + previous parts + separator
		//     -> skip all leading slashes ...
		while (*name == '/')
		{
			++name;
		}

		// ... then check for special part
		switch (*name)
		{
		case '\0':
			// STATE: name -- end of string
			//        outPtr -- trailing slash
			//        usize -- namespace + separator + previous parts + separator
			//     -> next iteration: loop will end, trailing slash will be replaced with terminator
			continue;
		case '.':
			switch (*(name + 1))
			{
			case '/':
			case '\0':
				// dot-part -> skip
				++name;
				// STATE: name -- part separator
				//        outPtr -- trailing slash
				//        usize -- namespace + separator + previous parts + separator
				//     -> next iteration: trailing slash will be overwritten
				continue;
			case '.':
				if (*(name + 2) == '/' || *(name + 2) == '\0')
				{
					// dot-dot-part -> go back one part

					// 1. terminate outPtr so that findStartOfLastPart works
					*outPtr = '\0';

					// 2. find start of previous part
					char * newOutPtr = findStartOfLastPart (*canonicalName, outPtr - *canonicalName);

					// 3. calculate unescaped length of part, including separator
					size_t ulen = outPtr - newOutPtr - 1;

					// 4. adjust pointers
					name += 2;
					outPtr = newOutPtr + 1;

					// 5. if previous part is empty ('%') ...
					if (ulen == 2 && *(newOutPtr + 1) == '%')
					{
						// 5a. ... then adjust len
						ulen = 1;
					}
					else
					{
						// 5b. ... else account of escape sequences
						size_t escapes = 0;
						for (size_t i = 1; i + 1 < ulen; ++i)
						{
							if (*(newOutPtr + i) == '\\')
							{
								++escapes;
								++i; // skip next character
							}
						}
						ulen -= escapes;
					}

					// 6. adjust usize
					ELEKTRA_ASSERT (ulen <= usize - 2, "usize underflow");
					usize -= ulen;

					// STATE: name -- part separator
					//        outPtr -- start of this part
					//        usize -- namespace + separator + parts before previous part + separator
					//     -> next iteration: part will be overwritten
					continue;
				}
				break;
			}
			break;
		case '%':
			if (*(name + 1) == '/' || *(name + 1) == '\0')
			{
				// empty part -> copy but only count / in usize
				++name;
				*outPtr++ = '%';
				*outPtr++ = '/';
				usize++;

				// STATE: name -- on part separator or end of string
				//        outPtr -- after last part
				//        usize -- namespace + separator + previous parts + separator + (empty part) + separator
				continue;
			}
			break;
		case '#': {
			// find end of digits
			const char * end = name + 1;
			while (isdigit (*end))
			{
				++end;
			}
			size_t len = end - name - 1;

			bool check1 = *end == '/' || *end == '\0';
			bool check2 = *(name + 1) != '0';

			if (len > 0 && check1 && check2 && (len < 19 || (len == 19 && strncmp (name + 1, "9223372036854775807", 19) <= 0)))
			{
				// non-canonical array part -> add underscores

				// but first update buffer
				size_t pos = outPtr - *canonicalName;

				*canonicalSizePtr += offset + len - 1;
				elektraRealloc ((void **) canonicalName, *canonicalSizePtr);
				outPtr = *canonicalName + pos;

				*outPtr = '#';
				memset (outPtr + 1, '_', len - 1);
				memcpy (outPtr + len, name + 1, len);
				outPtr += 2 * len;
				*outPtr++ = '/';
				usize += 2 * len + 1;
				name = end;

				// STATE: name -- part separator
				//        outPtr -- after this part
				//        usize -- namespace + separator + previous parts + separator + current part + separator
				//     -> next iteration: new part will be started
				continue;
			}
			break;
		}
		}

		// STATE: name -- first char of non-special part
		//        outPtr -- start of new part
		//        usize -- previous parts + separator
		//     -> find end of part
		const char * end = name;
		while (*end != '\0')
		{
			size_t backslashes = 0;
			while (*end == '\\')
			{
				++backslashes;
				++end;
			}

			usize += backslashes / 2;
			if (*end == '\0' || (*end == '/' && backslashes % 2 == 0))
			{
				break;
			}

			++end;
			usize += 1;
		}

		size_t len = end - name;

		// STATE: name -- start of part
		//        end -- end of part
		//        outPtr -- start of new part
		//        usize -- previous parts + separator + current part
		//     -> copy part +  part separator
		memcpy (outPtr, name, len);
		outPtr += len;
		*outPtr++ = '/';
		usize += 1;
		name += len;

		// STATE: name -- on part separator or end of string
		//        outPtr -- after last part
		//        usize -- namespace + separator + previous parts + separator + current part + separator
	}

	// terminate
	if (outPtr > *canonicalName + rootOffset + 1)
	{
		// not a root key -> need to replace trailing slash
		--outPtr;
	}
	else if (offset == 0 || usize == 2)
	{
		// root key -> don't replace slash, need to adjust usize
		++usize;
	}
	*outPtr = '\0';

	// output size and shrink buffer
	*canonicalSizePtr = outPtr - *canonicalName + 1;
	elektraRealloc ((void **) canonicalName, *canonicalSizePtr);

	// output unescape size if requested
	if (usizePtr != NULL)
	{
		*usizePtr = usize;
	}
}

/**
 * Takes a canonical key name and unescapes it.
 *
 * @param canonicalName The canonical name to unescape
 * @param unescapedName Output buffer for the unescaped name
 *
 * @pre @p canonicalName MUST be a canonical key name. If this is not the case, the result is undefined.
 * @pre @p unescapedName MUST be allocated to the correct size.
 *
 * @see elektraKeyNameCanonicalize
 *
 * @ingroup keyname
 */
void elektraKeyNameUnescape (const char * canonicalName, char * unescapedName)
{
	char * outPtr = unescapedName;

	if (canonicalName[0] != '/')
	{
		// translate namespace
		const char * colon = strchr (canonicalName, ':');
		*outPtr++ = elektraReadNamespace (canonicalName, colon - canonicalName);
		canonicalName = colon + 1;
	}
	else
	{
		// set cascading namespace
		*outPtr++ = KEY_NS_CASCADING;
	}

	while (*canonicalName != '\0')
	{
		switch (*canonicalName)
		{
		case '\\':
			// escape sequence: skip backslash ...
			++canonicalName;

			// ... and output escaped char
			*outPtr++ = *canonicalName;
			++canonicalName;
			break;
		case '/':
			// new part: output zero-byte part separator and ...
			*outPtr++ = '\0';
			++canonicalName;

			// ... check for empty part
			if (*canonicalName == '%' && (*(canonicalName + 1) == '/' || *(canonicalName + 1) == '\0'))
			{
				// skip percent
				++canonicalName;
			}
			break;
		default:
			// nothing special, just output
			*outPtr++ = *canonicalName;
			++canonicalName;
			break;
		}
	}

	// terminate
	*outPtr = '\0';
}

/**
 * Returns a pointer to the unescaped Key's name where the basename starts.
 *
 * This is a much more efficient version of keyGetBaseName() and you should
 * use it if you are responsible enough to not mess up things. The name might
 * change or even point to a wrong place after a keySetName(). So make
 * sure to copy the memory before the name changes.
 *
 * keyBaseName() returns "" when the Key has no basename. The reason is
 * @snippet testabi_key.c base0 empty
 *
 * There is also support for really empty basenames:
 * @snippet testabi_key.c base1 empty
 *
 * @note You must never use the pointer returned by keyBaseName()
 * method to change the name. You should use keySetBaseName()
 * instead.
 *
 * @note Do not assume that keyBaseName() points to the same region as
 * keyName() does.
 *
 * @param key the Key to obtain the basename from
 *
 * @return a pointer to the Key's basename
 * @retval "" when the Key has no (base)name
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetBaseName() for getting a copy of the Key's basename
 * @see keyGetBaseNameSize() for getting the size of the Key's basename
 * @see keyName() for getting a pointer to the Key's name
 */
const char * keyBaseName (const Key * key)
{
	if (!key) return 0;
	if (!key->key) return "";

	const char * baseName = key->ukey + key->keyUSize - 2;
	while (*baseName != '\0')
	{
		--baseName;
	}
	return baseName + 1;
}


/**
 * Calculates number of bytes needed to store basename of @p key (including
 * NULL terminator).
 *
 * Key names consisting of only root names (e.g. @c "system:/" or @c "user:/"
 * or @c "user:domain" ) do not have basenames. In this case the function will
 * return 1, because only a NULL terminator is needed for storage.
 *
 * Basenames are denoted as:
 * - @c system:/some/thing/basename -> @c basename
 * - @c user:domain/some/thing/base\\/name > @c base\\/name
 *
 * @param key the Key to get the size of the basename from
 *
 * @return size in bytes of the Key's basename including NULL terminator
 * @retval -1 if the Key or the Key's basename is NULL
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyBaseName() for getting a pointer to a Key's basename
 * @see keyGetBaseName() for getting a copy of a Key's basename
 * @see keyName(), keyGetName() for getting a pointer / copy of the whole name
 * @see keySetName() for setting a Key's name
 */
ssize_t keyGetBaseNameSize (const Key * key)
{
	const char * baseName = keyBaseName (key);
	if (!baseName) return -1;

	return elektraStrLen (baseName);
}


/**
 * Copy the Key's basename to @p returned
 *
 * The copy will include a NULL terminator which will be considered for the
 * returned size. Nothing will be copied if @p maxSize is smaller than the size
 * of the basename.
 *
 * Some examples:
 * - basename of @c system:/some/keyname is @c keyname
 * - basename of @c "user:/tmp/some key" is @c "some key"
 *
 * @param key the Key to extract basename from
 * @param returned a pre-allocated buffer for storing the basename
 * @param maxSize size of the buffer @p returned
 *
 * @return number of bytes copied to @p returned
 * @retval 1 when Key's name is empty
 * @retval -1 on NULL pointers
 * @retval -1 when maxSize is 0 or larger than SSIZE_MAX
 * @retval -1 when maxSize is smaller than the size of the Key's basename
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyBaseName() for getting a pointer to the Key's basename
 * @see keyGetBaseNameSize() for getting the size of a Key's basename
 * @see keyName(), keyGetName() for getting a pointer / copy of the whole name
 * @see keySetName() for setting a Key's name
 */
ssize_t keyGetBaseName (const Key * key, char * returned, size_t maxSize)
{
	if (key == NULL || returned == NULL) return -1;
	if (maxSize == 0 || maxSize > SSIZE_MAX) return -1;

	if (key->key == NULL)
	{
		*returned = '\0';
		return 1;
	}

	const char * baseName = keyBaseName (key);
	if (baseName == NULL)
	{
		return -1;
	}

	size_t baseSize = strlen (baseName) + 1;
	if (baseSize > maxSize)
	{
		return -1;
	}

	memcpy (returned, baseName, baseSize);
	return baseSize;
}

/**
 * Takes a single key name part and produces its escaped form.
 *
 * @param part A single key name part, i.e. contained '/' will be escaped, '\0' terminates part
 * @param escapedPart Output buffer for the escaped form
 *
 * @pre @p escapedPart MUST be a valid first argument for elektraRealloc() when cast to `void**`
 *
 * @returns The size of the escaped form excluding the zero terminator
 */
size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart)
{
	if (!part) return 0;

	size_t partLen = strlen (part);

	// first check for special parts
	if (partLen == 0)
	{
		// actually empty part
		elektraRealloc ((void **) escapedPart, 2);
		strcpy (*escapedPart, "%");
		return 1;
	}

	switch (part[0])
	{
	case '%':
		if (partLen == 1)
		{
			// escaped empty part
			elektraRealloc ((void **) escapedPart, 3);
			strcpy (*escapedPart, "\\%");
			return 2;
		}
		break;
	case '.':
		switch (part[1])
		{
		case '\0':
			// dot part
			elektraRealloc ((void **) escapedPart, 3);
			strcpy (*escapedPart, "\\.");
			return 2;
		case '.':
			if (partLen == 2)
			{
				// dot-dot part
				elektraRealloc ((void **) escapedPart, 4);
				strcpy (*escapedPart, "\\..");
				return 3;
			}
			break;
		}
		break;
	case '#':
		if (partLen > 1)
		{
			// possibly (non-)canonical array part
			size_t digits = 0;
			while (isdigit (part[1 + digits]))
			{
				++digits;
			}

			if (digits > 1 && part[1] != '0' &&
			    (digits < 19 || (digits == 19 && strncmp (&part[1], "9223372036854775807", 19) <= 0)))
			{
				// non-canonical array part -> need to escape
				elektraRealloc ((void **) escapedPart, partLen + 2);
				**escapedPart = '\\';
				memcpy (*escapedPart + 1, part, partLen + 1);
				return partLen + 1;
			}
		}
		break;
	}

	// not a special part -> just escape backslash and slash
	size_t special = 0;
	const char * cur = part;
	while ((cur = strpbrk (cur, "/\\")) != NULL)
	{
		++special;
		++cur;
	}

	elektraRealloc ((void **) escapedPart, partLen + special + 1);

	cur = part;
	char * outPtr = *escapedPart;
	while (*cur != '\0')
	{
		if (*cur == '/' || *cur == '\\')
		{
			*outPtr++ = '\\';
		}
		*outPtr++ = *cur++;
	}
	*outPtr = '\0';

	return outPtr - *escapedPart;
}

/**
 * Internal helper for keyAddBaseName() and keySetBaseName()
 */
static size_t keyAddBaseNameInternal (Key * key, const char * baseName)
{
	size_t unescapedSize;
	size_t escapedSize;
	char * escaped = NULL;
	int hasPath = key->keyUSize > 3;

	if (baseName == NULL)
	{
		// for keySetBaseName
		unescapedSize = 0;
		escapedSize = 0;
	}
	else
	{
		unescapedSize = strlen (baseName);
		escapedSize = elektraKeyNameEscapePart (baseName, &escaped);
		if (escapedSize == 0)
		{
			// error
			return -1;
		}

		if (hasPath)
		{
			// more than just namespace -> must add separator
			++escapedSize;
			++unescapedSize;
		}
	}

	size_t newKeySize = key->keySize + escapedSize;
	size_t newKeyUSize = key->keyUSize + unescapedSize;
	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and copy to malloced buffer
		char * tmp = elektraMalloc (newKeySize);
		memcpy (tmp, key->key, key->keySize);
		key->key = tmp;

		tmp = elektraMalloc (newKeyUSize);
		memcpy (tmp, key->ukey, key->keyUSize);
		key->ukey = tmp;

		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}
	else
	{
		elektraRealloc ((void **) &key->key, newKeySize);
		elektraRealloc ((void **) &key->ukey, newKeyUSize);
	}

	if (baseName == NULL)
	{
		// for keySetBaseName
		key->key[key->keySize - 1] = '\0';
		key->ukey[key->keyUSize - 1] = '\0';

		return key->keySize;
	}

	// add escaped name
	if (hasPath)
	{
		// more than just namespace -> must add separator
		key->key[key->keySize - 1] = '/';
		memcpy (&key->key[key->keySize], escaped, escapedSize);
	}
	else
	{
		memcpy (&key->key[key->keySize - 1], escaped, escapedSize);
	}
	elektraFree (escaped);

	// set keySize and terminate escaped name
	key->keySize += escapedSize;
	key->key[key->keySize - 1] = '\0';

	// add unescaped name
	if (hasPath)
	{
		// more than just namespace -> must add separator
		key->ukey[key->keyUSize - 1] = '\0';
		memcpy (&key->ukey[key->keyUSize], baseName, unescapedSize);
	}
	else
	{
		memcpy (&key->ukey[key->keyUSize - 1], baseName, unescapedSize);
	}

	// set keyUSize and terminate escaped name
	key->keyUSize += unescapedSize;
	key->ukey[key->keyUSize - 1] = '\0';

	set_bit (key->flags, KEY_FLAG_SYNC);
	return key->keySize;
}

/**
 * Adds @p baseName to the name of @p key.
 *
 * @p baseName will be escaped before adding it to the name of @p key.
 * No other part of the Key's name will be affected.
 *
 * Assumes that @p key is a directory and will append @p baseName to it.
 * The function adds the path separator for concatenating.
 *
 * If @p key has the name @c "system:/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have the name
 * @c "system:/dir1/dir2/mykey".
 *
 * When @p baseName is 0, nothing will happen and the size of the name is returned.
 *
 * The escaping rules apply as in @link keyname above @endlink.
 *
 * A simple example is:
 * @snippet keyBasename.c add base basic
 *
 * E.g. if you add . it will be escaped:
 * @snippet testabi_key.c base1 add
 *
 * @param key the Key to add the basename to
 * @param baseName the string to append to the Key's name
 *
 * @return the size in bytes of the Key's new name including the NULL terminator
 * @retval -1 if the Key has no name
 * @retval -1 on NULL pointers
 * @retval -1 if Key was inserted into KeySet before
 * @retval -1 if the Key was read-only
 * @retval -1 on memory allocation errors
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keySetBaseName() for setting the basename of a Key
 * @see keySetName() for setting the name of a key
 *
 */
ssize_t keyAddBaseName (Key * key, const char * baseName)
{
	if (!key) return -1;
	if (!baseName) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	return keyAddBaseNameInternal (key, baseName);
}

/**
 * Sets @p baseName as the new basename for @p key.
 *
 * Only the basename of the Key will be affected.
 *
 * A simple example is:
 * @snippet keyBasename.c set base basic
 *
 * All text after the last @c '/' in the Key's name is erased and
 * @p baseName is appended.
 * If @p baseName is 0 (NULL), then the last part of the Key's name is
 * removed without replacement. The root name of the Key will not be removed though.
 *
 * Let us suppose @p key has name @c "system:/dir1/dir2/key1". If @p baseName
 * is @c "key2", the resulting key name will be @c "system:/dir1/dir2/key2".
 * If @p baseName is 0 (NULL), the resulting key name will
 * be @c "system:/dir1/dir2".
 * If @p baseName is empty, the resulting key name will
 * be @c "system:/dir1/dir2/%", where @c "%" denotes an empty base name,
 * as also shown in the following code:
 *
 * @snippet testabi_key.c base2
 *
 * keySetBaseName() does proper escaping on the supplied name argument.
 *
 * You can use character sequences as @c baseName (e.g. @c "." (dot), @c ".."
 * (dot-dot), @c "%" (empty basename)). They will be properly escaped
 * and will not have their usual meaning.
 *
 * If you want to add to the basename instead of changing it, use keyAddBaseName().
 * If you do not want any escaping, use keyAddName().
 *
 * @param key the Key whose basename to set
 * @param baseName the new basename for the Key
 *
 * @return the size in bytes of the new key name
 * @retval -1 if Key is NULL
 * @retval -1 if Key was inserted into KeySet before
 * @retval -1 if Key is read-only
 * @retval -1 on allocation errors
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyAddBaseName() for adding a basename instead of changing it
 * @see keyAddName() for adding a name without escaping
 * @see keySetName() for setting a completely new name
 * @see keyname for more details on special names
 */
ssize_t keySetBaseName (Key * key, const char * baseName)
{
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	// adjust sizes to exclude base name
	const char * baseNamePtr = findStartOfLastPart (key->key, key->keySize);
	if (baseNamePtr == NULL)
	{
		return -1;
	}
	key->keySize = baseNamePtr - key->key + 1;

	const char * ubaseNamePtr = key->ukey + key->keyUSize - 2;
	while (*ubaseNamePtr != '\0')
	{
		--ubaseNamePtr;
	}
	key->keyUSize = ubaseNamePtr - key->ukey + 1;

	if (key->keyUSize == 2)
	{
		// happens if we only have one part after namespace
		++key->keySize;
		++key->keyUSize;
	}

	// add new base name, only resizes buffer when baseName == NULL
	return keyAddBaseNameInternal (key, baseName);
}

/**
 * Returns the #elektraNamespace for a Key.
 *
 * To handle every namespace a Key could have, you can use the following snippet:
 * @snippet namespace.c namespace
 *
 * To loop over all valid namespaces use:
 * @snippet namespace.c loop
 *
 * @note This method might be extended. There is no guarantee that a Key with
 * a specific namespace will retain that namespace after recompilation.
 * Make sure that your compiler gives you a warning
 * for unhandled switches (gcc: -Wswitch or -Wswitch-enum if you
 * want to handle default) and look out for those warnings when recompiling.
 *
 * @param key the Key to get the namespace from
 *
 * @return the namespace of the Key
 * @retval KEY_NS_NONE if Key is NULL
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keySetNamespace() for setting a Key's namespace
 */
elektraNamespace keyGetNamespace (const Key * key)
{
	if (!key) return KEY_NS_NONE;
	return (elektraNamespace) key->ukey[0];
}

/**
 * Changes the namespace of a Key.
 *
 * The rest of the Key's name remains unchanged.
 *
 * @pre @p ns MUST be a valid namespace and not #KEY_NS_NONE
 * @pre @p key MUST be a valid #Key, especially `key != NULL`
 *
 * @param key The #Key whose namespace will be changed
 * @param ns  The new namespace of for @p key
 *
 * @return the new size in bytes of the Key's namespace
 * @retval -1 precondition error
 *
 * @since 1.0.0
 * @ingroup keyname
 * @see keyGetNamespace() for getting a Key's namespace
 */
ssize_t keySetNamespace (Key * key, elektraNamespace ns)
{
	if (!key) return -1;
	if (ns == KEY_NS_NONE) return -1;

	if (ns == key->ukey[0]) return key->keySize;

	size_t oldNamespaceLen;
	switch (key->ukey[0])
	{
	case KEY_NS_USER:
		oldNamespaceLen = sizeof ("user:") - 1;
		break;
	case KEY_NS_SYSTEM:
		oldNamespaceLen = sizeof ("system:") - 1;
		break;
	case KEY_NS_DIR:
		oldNamespaceLen = sizeof ("dir:") - 1;
		break;
	case KEY_NS_META:
		oldNamespaceLen = sizeof ("meta:") - 1;
		break;
	case KEY_NS_SPEC:
		oldNamespaceLen = sizeof ("spec:") - 1;
		break;
	case KEY_NS_PROC:
		oldNamespaceLen = sizeof ("proc:") - 1;
		break;
	case KEY_NS_CASCADING:
		oldNamespaceLen = 0;
		break;
	case KEY_NS_DEFAULT:
		oldNamespaceLen = sizeof ("default:") - 1;
		break;
	default:
		return -1;
	}

	const char * newNamespace;
	switch (ns)
	{
	case KEY_NS_USER:
		newNamespace = "user:";
		break;
	case KEY_NS_SYSTEM:
		newNamespace = "system:";
		break;
	case KEY_NS_DIR:
		newNamespace = "dir:";
		break;
	case KEY_NS_META:
		newNamespace = "meta:";
		break;
	case KEY_NS_SPEC:
		newNamespace = "spec:";
		break;
	case KEY_NS_PROC:
		newNamespace = "proc:";
		break;
	case KEY_NS_CASCADING:
		newNamespace = "";
		break;
	case KEY_NS_DEFAULT:
		newNamespace = "default:";
		break;
	default:
		return -1;
	}

	size_t newNamespaceLen = strlen (newNamespace);

	if (newNamespaceLen > oldNamespaceLen)
	{
		// buffer growing -> realloc first
		elektraRealloc ((void **) &key->key, key->keySize - oldNamespaceLen + newNamespaceLen);
	}

	memmove (key->key + newNamespaceLen, key->key + oldNamespaceLen, key->keySize - oldNamespaceLen);

	if (newNamespaceLen < oldNamespaceLen)
	{
		// buffer growing -> realloc after
		elektraRealloc ((void **) &key->key, key->keySize - oldNamespaceLen + newNamespaceLen);
	}

	memcpy (key->key, newNamespace, newNamespaceLen);
	key->keySize += newNamespaceLen;
	key->keySize -= oldNamespaceLen;
	key->key[key->keySize - 1] = '\0';

	key->ukey[0] = ns;

	return key->keySize;
}


/**
 * @}
 */
