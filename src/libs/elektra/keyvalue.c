/**
 * @file
 *
 * @brief Methods for Key value manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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

#if DEBUG && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#include "kdbprivate.h"


/**
 * @defgroup keyvalue Value Manipulation Methods
 * @ingroup key
 * @brief Methods to do various operations on Key values.
 *
 * A key can contain a value in different format. The most
 * likely situation is, that the value is interpreted as
 * text. Use keyGetString() for that.
 * You can save any Unicode Symbols and Elektra will
 * take care that you get the same back, independent of
 * your current environment.
 *
 * In some situations this idea fails. When you need exactly
 * the same value back without any interpretation of the
 * characters, there is keySetBinary(). If you use that, its
 * very likely that your Configuration is not according
 * to the standard. Also for Numbers, Booleans and Date you
 * should use keyGetString(). To do so, you might use strtod()
 * strtol() and then atol() or atof() to convert back.
 *
 * To use them:
 * @code
#include <elektra/kdb.h>
 * @endcode
 *
 *
 */


/*******************************************
 *    General value manipulation methods   *
 *******************************************/


/**
 * Return a pointer to the real internal @p key value.
 *
 * This is a much more efficient version of keyGetString()
 * keyGetBinary(), and you should use it if you are responsible enough to
 * not mess up things. You are not allowed to modify anything
 * in the returned string. If you need a copy of the Value, consider
 * to use keyGetString() or keyGetBinary() instead.
 *
 * @section string String Handling
 *
 * If @p key is string (keyIsString()), you may cast the returned as a
 * @c "char *" because you'll get a NULL terminated regular string.
 *
 * keyValue() returns "" in string mode when there is no value. The reason is
 * @code
key=keyNew(0);
keySetString(key,"");
keyValue(key); // you would expect "" here
keyDel(key);
 * @endcode
 *
 * @section binary Binary Data Handling
 *
 * If the data is binary, the size of the value must be determined by
 * keyGetValueSize(), any strlen() operations are not suitable to determine
 * the size.
 *
 * keyValue() returns 0 in binary mode when there is no value. The reason is
 * @code
key=keyNew(0);
keySetBinary(key, 0, 0);
keyValue(key); // you would expect 0 here

keySetBinary(key,"", 1);
keyValue(key); // you would expect "" (a pointer to '\0') here

int i=23;
keySetBinary(key, (void*)&i, 4);
(int*)keyValue(key); // you would expect a pointer to (int)23 here
keyDel(key);
 * @endcode
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyValue() method to set a new
 * value. Use keySetString() or keySetBinary() instead.
 *
 * @warning Binary keys will return a NULL pointer when there is no data in contrast
 * to keyName(), keyBaseName(), keyOwner() and keyComment(). For string value the
 * behaviour is the same.
 *
 * @par Example:
 * @code
KDB *handle = kdbOpen();
KeySet *ks=ksNew(0, KS_END);
Key *current=0;

kdbGetByName(handle,ks,"system/sw/my",KDB_O_SORT|KDB_O_RECURSIVE);

ksRewind(ks);
while (current=ksNext(ks)) {
	size_t size=0;

	if (keyIsBin(current)) {
		size=keyGetValueSize(current);
		printf("Key %s has a value of size %d bytes. Value: <BINARY>\nComment: %s",
			keyName(current),
			size,
			keyComment(current));
	} else {
		size=elektraStrLen((char *)keyValue(current));
		printf("Key %s has a value of size %d bytes. Value: %s\nComment: %s",
			keyName(current),
			size,
			(char *)keyValue(current),
			keyComment(current));
	}
}

ksDel (ks);
kdbClose (handle);
 * @endcode
 *
 * @param key the key object to work with
 * @return a pointer to internal value
 * @retval "" when there is no data and key is not binary
 * @retval 0 where there is no data and key is binary
 * @retval 0 on NULL pointer
 * @see keyGetValueSize(), keyGetString(), keyGetBinary()
 * @ingroup keyvalue
 */
const void * keyValue (const Key * key)
{
	if (!key) return 0;

	if (!key->data.v)
	{
		/*errno=KDB_ERR_NOKEY;*/
		if (keyIsBinary (key))
			return 0;
		else
			return "";
	}

	return key->data.v;
}


/**
 * Get the c-string representing the value.
 *
 * Will return (null) on null pointers.
 * Will return (binary) on binary data not ended
 * with a null byte.
 *
 * It is not checked if it is actually a string,
 * only if it terminates for security reasons.
 *
 * @return the c-string of the value
 * @retval "(null)" on null keys
 * @retval "" if no data found
 * @retval "(binary)" on binary keys
 * @ingroup keyvalue
 * @param key the key object to get the string from
 */
const char * keyString (const Key * key)
{
	if (!key) return "(null)";

	if (!key->data.c)
	{
		return "";
	}

	if (keyIsBinary (key))
	{
		return "(binary)";
	}

	return key->data.c;
}


/**
 * Returns the number of bytes needed to store the key value, including the
 * NULL terminator.
 *
 * It returns the correct size, independent of the Key Type.
 * If it is a binary there might be '\\0' values in it.
 *
 * For an empty string you need one byte to store the ending NULL.
 * For that reason 1 is returned. This is not true for binary data,
 * so there might be returned 0 too.
 *
 * A binary key has no '\\0' termination. String types have it, so to there
 * length will be added 1 to have enough space to store it.
 *
 * This method can be used with elektraMalloc() before keyGetString() or keyGetBinary()
 * is called.
 *
 * @code
char *buffer;
buffer = elektraMalloc (keyGetValueSize (key));
// use this buffer to store the value (binary or string)
// pass keyGetValueSize (key) for maxSize
 * @endcode
 *
 * @param key the key object to work with
 * @return the number of bytes needed to store the key value
 * @retval 1 when there is no data and type is not binary
 * @retval 0 when there is no data and type is binary
 * @retval -1 on null pointer
 * @see keyGetString(), keyGetBinary(), keyValue()
 * @ingroup keyvalue
 */
ssize_t keyGetValueSize (const Key * key)
{
	if (!key) return -1;

	if (!key->data.v)
	{
		/*errno=KDB_ERR_NODATA;*/
		if (keyIsBinary (key))
			return 0;
		else
			return 1;
	}

	return key->dataSize;
}


/**
 * Get the value of a key as a string.
 *
 * When there is no value inside the string, 1 will
 * be returned and the returnedString will be empty
 * "" to avoid programming errors that old strings are
 * shown to the user.
 *
 * For binary values see keyGetBinary() and keyIsBinary().
 *
 * @par Example:
 * @code
Key *key = keyNew ("user/keyname", KEY_END);
char buffer[300];

if (keyGetString(key,buffer,sizeof(buffer)) == -1)
{
	// handle error
} else {
	printf ("buffer: %s\n", buffer);
}
 * @endcode
 *
 * @param key the object to gather the value from
 * @param returnedString pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of allocated memory in @p returnedString
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @retval 1 if the string is empty
 * @retval -1 on any NULL pointers
 * @retval -1 on type mismatch: string expected, but found binary
 * @retval -1 maxSize is 0
 * @retval -1 if maxSize is too small for string
 * @retval -1 if maxSize is larger than SSIZE_MAX
 * @see keyValue(), keyGetValueSize(), keySetString(), keyString()
 * @see keyGetBinary() for working with binary data
 * @ingroup keyvalue
 */
ssize_t keyGetString (const Key * key, char * returnedString, size_t maxSize)
{
	if (!key) return -1;

	if (!maxSize) return -1;
	if (!returnedString) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	if (!keyIsString (key))
	{
		/*errno=KDB_ERR_TYPEMISMATCH;*/
		return -1;
	}

	if (!key->data.v)
	{
		returnedString[0] = 0;
		return 1;
	}

	if (key->dataSize > maxSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}


	strncpy (returnedString, key->data.c, maxSize);
	return key->dataSize;
}


/**
 * Set the value for @p key as @p newStringValue.
 *
 * The function will allocate and save a private copy of @p newStringValue, so
 * the parameter can be freed after the call.
 *
 * String values will be saved in backend storage, when kdbSetKey() will be
 * called, in UTF-8 universal encoding, regardless of the program's current
 * encoding, when iconv plugin is present.
 *
 * @note The type will be set to KEY_TYPE_STRING.
 * When the type of the key is already a string type it won't be changed.
 *
 * @param key the key to set the string value
 * @param newStringValue NULL-terminated text string to be set as @p key's
 * 	value
 * @return the number of bytes actually saved in private struct including final
 * 	NULL
 * @retval 1 if newStringValue is a NULL pointer, this will make the
 *           string empty (string only containing null termination)
 * @retval -1 if key is a NULL pointer
 * @see keyGetString(), keyValue(), keyString()
 * @ingroup keyvalue
 */
ssize_t keySetString (Key * key, const char * newStringValue)
{
	ssize_t ret = 0;

	if (!key) return -1;

	keySetMeta (key, "binary", 0);

	if (!newStringValue || newStringValue[0] == '\0')
		ret = keySetRaw (key, 0, 0);
	else
		ret = keySetRaw (key, newStringValue, elektraStrLen (newStringValue));

	keySetMeta (key, "origvalue", 0);

	return ret;
}


/**
 * Get the value of a key as a binary.
 *
 * If the type is not binary -1 will be returned.
 *
 * When the binary data is empty (this is not the same as ""!)
 * 0 will be returned and the returnedBinary will not be changed.
 *
 * For string values see keyGetString() and keyIsString().
 *
 * When the returnedBinary is to small to hold the data
 * (its maximum size is given by maxSize),
 * the returnedBinary will not be changed and -1 is returned.
 *
 * @par Example:
 * @code
Key *key = keyNew ("user/keyname", KEY_TYPE, KEY_TYPE_BINARY, KEY_END);
char buffer[300];

if (keyGetBinary(key,buffer,sizeof(buffer)) == -1)
{
	// handle error
}
 * @endcode
 *
 * @param key the object to gather the value from
 * @param returnedBinary pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of pre-allocated memory in @p returnedBinary
 * @return the number of bytes actually copied to @p returnedBinary
 * @retval 0 if the binary is empty
 * @retval -1 on NULL pointers
 * @retval -1 if maxSize is 0
 * @retval -1 if maxSize is too small for string
 * @retval -1 if maxSize is larger than SSIZE_MAX
 * @retval -1 on type mismatch: binary expected, but found string
 * @see keyValue(), keyGetValueSize(), keySetBinary()
 * @see keyGetString() and keySetString() as preferred alternative to binary
 * @see keyIsBinary() to see how to check for binary type
 * @ingroup keyvalue
 */
ssize_t keyGetBinary (const Key * key, void * returnedBinary, size_t maxSize)
{
	if (!key) return -1;
	if (!returnedBinary) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!keyIsBinary (key))
	{
		/*errno=KDB_ERR_TYPEMISMATCH;*/
		return -1;
	}

	if (!key->data.v)
	{
		return 0;
	}

	if (key->dataSize > maxSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}

	memcpy (returnedBinary, key->data.v, key->dataSize);
	return key->dataSize;
}


/**
 * Set the value of a key as a binary.
 *
 * A private copy of @p newBinary will allocated and saved inside @p key,
 * so the parameter can be deallocated after the call.
 *
 * Binary values might be encoded in another way then string values
 * depending on the plugin. Typically character encodings should not take
 * place on binary data.
 * Consider using a string key instead.
 *
 * When newBinary is a NULL pointer the binary will be freed and 0 will
 * be returned.
 *
 * @note The metadata "binary" will be set to mark that the key is
 * binary from now on. When the key is already binary the metadata
 * won't be changed. This will only happen in the successful case,
 * but not when -1 is returned.
 *
 * @param key the object on which to set the value
 * @param newBinary is a pointer to any binary data or NULL to free the previous set data
 * @param dataSize number of bytes to copy from @p newBinary
 * @return the number of bytes actually copied to internal struct storage
 * @retval 0 when the internal binary was freed and is now a null pointer
 * @retval -1 if key is a NULL pointer
 * @retval -1 when dataSize is 0 (but newBinary not NULL) or larger than SSIZE_MAX
 * @see keyGetBinary()
 * @see keyIsBinary() to check if the type is binary
 * @see keyGetString() and keySetString() as preferred alternative to binary
 * @ingroup keyvalue
 */
ssize_t keySetBinary (Key * key, const void * newBinary, size_t dataSize)
{
	ssize_t ret = 0;

	if (!key) return -1;

	if (!dataSize && newBinary) return -1;
	if (dataSize > SSIZE_MAX) return -1;
	if (key->flags & KEY_FLAG_RO_VALUE) return -1;

	keySetMeta (key, "binary", "");

	ret = keySetRaw (key, newBinary, dataSize);


	return ret;
}

/**
 * @internal
 *
 * Set raw  data as the value of a key.
 * If NULL pointers are passed, key value is cleaned.
 * This method will not change or set the key type, and should only
 * be used internally in elektra.
 *
 * @param key the key object to work with
 * @param newBinary array of bytes to set as the value
 * @param dataSize number bytes to use from newBinary, including the final NULL
 * @return The number of bytes actually set in internal buffer.
 * @retval 1 if it was a string which was deleted
 * @retval 0 if it was a binary which was deleted
 * @see keySetType(), keySetString(), keySetBinary()
 * @ingroup keyvalue
 */
ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize)
{
	if (!key) return -1;
	if (key->flags & KEY_FLAG_RO_VALUE) return -1;

	if (!dataSize || !newBinary)
	{
		if (key->data.v)
		{
			if (!test_bit (key->flags, KEY_FLAG_MMAP_DATA)) elektraFree (key->data.v);
			key->data.v = NULL;
			clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_DATA);
		}
		key->dataSize = 0;
		set_bit (key->flags, KEY_FLAG_SYNC);
		if (keyIsBinary (key)) return 0;
		return 1;
	}

	key->dataSize = dataSize;
	if (key->data.v)
	{
		char * previous = key->data.v;

		if (test_bit (key->flags, KEY_FLAG_MMAP_DATA))
		{
			clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_DATA);
			key->data.v = elektraMalloc (key->dataSize);
			if (key->data.v == NULL) return -1;
		}
		else
		{
			if (-1 == elektraRealloc ((void **) &key->data.v, key->dataSize)) return -1;
		}

		if (-1 == elektraRealloc ((void **) &key->data.v, key->dataSize)) return -1;
		if (previous == key->data.v)
		{
			// In case the regions overlap, use memmove to stay safe
			memmove (key->data.v, newBinary, key->dataSize);
		}
		else
		{
			memcpy (key->data.v, newBinary, key->dataSize);
		}
	}
	else
	{
		char * p = elektraMalloc (key->dataSize);
		if (NULL == p) return -1;
		key->data.v = p;
		memcpy (key->data.v, newBinary, key->dataSize);
	}

	set_bit (key->flags, KEY_FLAG_SYNC);
	return keyGetValueSize (key);
}
