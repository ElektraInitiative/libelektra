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
#include <kdb.h>
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
 * keyGetBinary(). You should use it if you are responsible enough to
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
 * to keyName(), keyBaseName() and keyComment(). For string value the
 * behaviour is the same.
 *
 * @par Example:
 * @code
KDB *handle = kdbOpen();
KeySet *ks=ksNew(0, KS_END);
kdbGetByName(handle,ks,"system:/sw/my",KDB_O_SORT|KDB_O_RECURSIVE);

 for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
 {
	Key * current = ksAtCursor (ks, it);
	size_t size=0;

	if (keyIsBinary(current)) {
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
 * @pre @p key is not NULL and has stored data
 * @post returned pointer points to the stored internal value
 * @post if the value is a string, the value is NULL terminated
 *
 * @param key the Key from which to get the value
 *
 * @return a pointer to the Key's internal value
 * @retval "" when there is no value and Key is not binary
 * @retval 0 where there is no value and Key is binary
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyGetValueSize() to get the size of the Key's value
 * @see keyGetString() for getting the Key's value as string
 * @see keyGetBinary() for getting the Key's value as binary
 */
const void * keyValue (const Key * key)
{
	if (!key) return NULL;

	if (!key->keyData || !key->keyData->data.v)
	{
		if (keyIsBinary (key))
			return NULL;
		else
			return "";
	}

	return key->keyData->data.v;
}


/**
 * Get a pointer to the c-string representing the value.
 *
 * Will return "(null)" on null pointers.
 * Will return "(binary)" on binary data not ended
 * with a null byte.
 *
 * @note You must not change or delete the returned value. Use the respective
 * functions for that (keySetString(), keyGetString())
 *
 * It is not checked if it is actually a string,
 * only if it terminates for security reasons.
 *
 * @post returned pointer points to the real internal value
 * @post value is NULL terminated
 *
 * @param key the Key object to get the string value from
 *
 * @return pointer to the c-string representing the Key's value
 * @retval "" if no data found
 * @retval "(null)" on null Key
 * @retval "(binary)" on binary Key
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyGetString() for getting a copy of the Key's value as string
 * @see keyGetBinary() for getting a copy of the Key's value as binary
 * @see keyValue() for getting a pointer to the Key's value as binary
 */
const char * keyString (const Key * key)
{
	if (!key) return "(null)";

	if (!key->keyData || !key->keyData->data.c)
	{
		return "";
	}

	if (keyIsBinary (key))
	{
		return "(binary)";
	}

	return key->keyData->data.c;
}


/**
 * Returns the number of bytes needed to store the key value, including the
 * NULL terminator.
 *
 * It returns the correct size, independent of the Key Type.
 * If the value is binary there might be '\\0' values in it.
 *
 * For an empty string you need one byte to store the ending NULL.
 * For that reason 1 is returned. This is not true for binary data,
 * so 0 will be returned.
 *
 * A binary key has no '\\0' termination. String types are null-terminated,
 * and the terminator will be considered for the length.
 *
 * This method can be used with elektraMalloc() before
 * keyGetString() or keyGetBinary() is called.
 *
 * @code
char *buffer;
buffer = elektraMalloc (keyGetValueSize (key));
// use this buffer to store the value (binary or string)
// pass keyGetValueSize (key) for maxSize
 * @endcode
 *
 * @post returns the exact amount of bytes needed to store @p key's value
 * (including NULL terminators)
 *
 * @param key the Key object to get the size of the value from
 *
 * @return the number of bytes needed to store the Key's value
 * @retval 1 when there is no data and type is a string
 * @retval 0 when there is no data and type is binary
 * @retval -1 on null pointer
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyGetString() for getting the Key's value as a string
 * @see keyGetBinary() for getting the Key's value as a binary
 * @see keyValue() for getting a pointer to the Key's value
 */
ssize_t keyGetValueSize (const Key * key)
{
	if (!key) return -1;

	if (!key->keyData || !key->keyData->data.v)
	{
		if (keyIsBinary (key))
			return 0;
		else
			return 1;
	}

	return key->keyData->dataSize;
}


/**
 * Copy the string value of a Key into @p returnedString.
 *
 * When there is no value inside the string, 1 will
 * be returned and the returnedString will be empty
 * ("") to avoid programming errors where old strings are
 * shown to the user.
 *
 * @par Example:
 * @code
Key *key = keyNew ("user:/keyname", KEY_END);
char buffer[300];

if (keyGetString(key,buffer,sizeof(buffer)) == -1)
{
	// handle error
} else {
	printf ("buffer: %s\n", buffer);
}
 * @endcode
 *
 * @param key the Key object to get the string from
 * @param returnedString pre-allocated memory to store a copy of the Key's value
 * @param maxSize number of bytes of allocated memory in @p returnedString
 *
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @retval 1 if the string is empty
 * @retval -1 on any NULL pointers
 * @retval -1 if the Key's value is binary
 * @retval -1 maxSize is 0, too small for the string or larger than SSIZE_MAX
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyGetValueSize() for getting the size of the Key's value
 * @see keyValue() for getting a raw pointer to the Key's value
 * @see keyString() for getting a raw char pointer to the Key's value
 * @see keyGetBinary(), keyIsBinary() for working with binary data
 */
ssize_t keyGetString (const Key * key, char * returnedString, size_t maxSize)
{
	if (!key) return -1;

	if (!maxSize) return -1;
	if (!returnedString) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	if (!keyIsString (key))
	{
		return -1;
	}

	if (!key->keyData || !key->keyData->data.v)
	{
		returnedString[0] = 0;
		return 1;
	}

	if (key->keyData->dataSize > maxSize)
	{
		return -1;
	}


	strncpy (returnedString, key->keyData->data.c, maxSize);
	return key->keyData->dataSize;
}


/**
 * Set the value for @p key as @p newStringValue.
 *
 * The function will allocate and save a private copy of @p newStringValue, so
 * the parameter can be freed after the call.
 *
 * String values will be saved in backend storage in UTF-8 universal encoding,
 * regardless of the program's current encoding (if the iconv plugin is
 * available).
 *
 * @pre @p newStringValue is a NULL terminated string
 * @post Value of the Key is set to the UTF-8 encoded value of @p newStringValue
 * @post Metakey `meta:/binary` is cleared
 *
 * @param key the Key for which to set the string value
 * @param newStringValue NULL-terminated string to be set as @p key's
 * 	value
 *
 * @return the number of bytes actually saved in private struct including final
 * 	NULL
 * @retval 1 if @p newStringValue is a NULL pointer, this will make the
 *           string empty (string only containing null termination)
 * @retval -1 if @p key is a NULL pointer
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyString() for getting a pointer to the Key's value
 * @see keyGetString() for getting a copy of the Key's value
 * @see keySetBinary() for setting binary data
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
 * Copy the binary value of a Key into @p returnedBinary
 *
 * If the type is not binary -1 will be returned.
 *
 * When the binary data is empty (this is not the same as ""!)
 * 0 will be returned and @p returnedBinary will not be changed.
 *
 * For string values see keyGetString() and keyIsString().
 *
 * When @p returnedBinary is too small to hold the data
 * (maximum size is given by maxSize),
 * the returnedBinary will not be changed and -1 is returned.
 *
 * @par Example:
 * @code
Key *key = keyNew ("user:/keyname", KEY_BINARY, KEY_END);
char buffer[300];

if (keyGetBinary(key,buffer,sizeof(buffer)) == -1)
{
	// handle error
}
 * @endcode
 *
 * @param key the Key object to get the binary value from
 * @param returnedBinary pre-allocated memory to store a copy of the Key's value
 * @param maxSize number of bytes of pre-allocated memory in @p returnedBinary
 *
 * @return the number of bytes copied to @p returnedBinary
 * @retval 0 if the binary is empty
 * @retval -1 on NULL pointers
 * @retval -1 if maxSize is 0, too small for the value or larger than SSIZE_MAX
 * @retval -1 if the Key's value is a string
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyValue() for getting a raw pointer to the Key's value
 * @see keyGetValueSize() for getting the size of the Key's value
 * @see keySetBinary() for setting the binary value of a Key
 * @see keyIsBinary() for checking whether a Key's value is binary
 * @see keyGetString(), keySetString() for working with string values
 */
ssize_t keyGetBinary (const Key * key, void * returnedBinary, size_t maxSize)
{
	if (!key) return -1;
	if (!returnedBinary) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!keyIsBinary (key))
	{
		return -1;
	}

	if (!key->keyData || !key->keyData->data.v)
	{
		return 0;
	}

	if (key->keyData->dataSize > maxSize)
	{
		return -1;
	}

	memcpy (returnedBinary, key->keyData->data.v, key->keyData->dataSize);
	return key->keyData->dataSize;
}


/**
 * Set the value of a Key to the binary value @p newBinary
 *
 * A private copy of @p newBinary will be allocated and saved inside @p key,
 * so the parameter can be deallocated after the call.
 *
 * Binary values might be encoded in another way than string values
 * depending on the plugin. Typically character encodings should not take
 * place on binary data.
 * Consider using a string Key instead, if encoding should occur.
 *
 * When @p newBinary is a NULL pointer the value will be freed and 0 will
 * be returned.
 *
 * Read-only keys will stay unchanged after calling this function.
 *
 * @note The metadata "binary" will be set to mark that the key is
 * binary from now on. When the Key is already binary the metadata
 * won't be changed. This will only happen in the successful case,
 * but not when -1 is returned.
 *
 * @pre @p dataSize matches the length of @p newBinary
 * @pre @p newBinary is not NULL and @p dataSize > 0
 * @pre @p key is not read-only
 * @post @p key's value set exactly to the data in @p newBinary
 * @post "binary" key set in @p key's metadata
 *
 * @param key the Key object where the value should be set
 * @param newBinary a pointer to any binary data or NULL (to clear the stored value)
 * @param dataSize number of bytes to copy from @p newBinary
 *
 * @return the number of bytes actually copied to internal struct storage
 * @retval 0 when the internal binary was freed and is now a null pointer
 * @retval -1 if @p key is NULL
 * @retval -1 when @p dataSize is 0 (and newBinary not NULL) or larger than SSIZE_MAX
 * @retval -1 if @p key is read-only
 *
 * @since 1.0.0
 * @ingroup keyvalue
 * @see keyGetBinary() for getting a Key's value as binary
 * @see keyIsBinary() to check if the Key's value is binary
 * @see keyGetString() and keySetString() for working with string values
 */
ssize_t keySetBinary (Key * key, const void * newBinary, size_t dataSize)
{
	ssize_t ret = 0;

	if (!key) return -1;

	if (!dataSize && newBinary) return -1;
	if (dataSize > SSIZE_MAX) return -1;
	if (key->hasReadOnlyValue) return -1;

	keySetMeta (key, "binary", "");

	ret = keySetRaw (key, newBinary, dataSize);


	return ret;
}

static inline void keyDetachKeyDataWithoutCopy (Key * key)
{
	if (key == NULL)
	{
		return;
	}

	if (key->keyData == NULL)
	{
		key->keyData = keyDataNew ();
		keyDataRefInc (key->keyData);
	}
	else if (key->keyData->refs > 1 || isKeyDataInMmap (key->keyData))
	{
		keyDataRefDecAndDel (key->keyData);

		key->keyData = keyDataNew ();
		keyDataRefInc (key->keyData);
	}
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
	if (key->hasReadOnlyValue) return -1;

	keyDetachKeyDataWithoutCopy (key);

	if (!dataSize || !newBinary)
	{
		if (key->keyData->data.v)
		{
			elektraFree (key->keyData->data.v);
			key->keyData->data.v = NULL;
		}
		key->keyData->dataSize = 0;
		key->needsSync = true;
		if (keyIsBinary (key)) return 0;
		return 1;
	}

	key->keyData->dataSize = dataSize;
	if (key->keyData->data.v)
	{
		char * previous = key->keyData->data.v;

		if (-1 == elektraRealloc ((void **) &key->keyData->data.v, key->keyData->dataSize)) return -1;
		if (previous == key->keyData->data.v)
		{
			// In case the regions overlap, use memmove to stay safe
			memmove (key->keyData->data.v, newBinary, key->keyData->dataSize);
		}
		else
		{
			memcpy (key->keyData->data.v, newBinary, key->keyData->dataSize);
		}
	}
	else
	{
		char * p = elektraMalloc (key->keyData->dataSize);
		if (NULL == p) return -1;
		key->keyData->data.v = p;
		memcpy (key->keyData->data.v, newBinary, key->keyData->dataSize);
	}

	key->needsSync = true;
	return keyGetValueSize (key);
}
