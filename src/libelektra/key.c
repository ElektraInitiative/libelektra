 /***************************************************************************
                          key.c  -  Methods for Key manipulation
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


/**
 * @defgroup key Key :: Basic Methods
 * @brief Key construction and initialization methods.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * A Key is the essential class that encapsulates key @link keyname name @endlink,
 * @link keyvalue value @endlink and @link keymeta metainfo @endlink.
 * Key properties are:
 * - @link keyname Key name @endlink
 * - @link keyvalue Key value @endlink
 * - @link keySetType() Data type @endlink
 * - @link keyGetComment() Key comment @endlink
 * - @link keyGetOwner() Key owner @endlink
 * - @link keymeta UID, GID and filesystem-like mode permissions @endlink
 * - @link keymeta Mode, change and modification times @endlink
 *
 * Described here the methods to allocate and free the key.
 *
 */



#ifdef HAVE_CONFIG_H
#include "config.h"
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


/**
 * A practical way to fully create a Key object in one step.
 *
 * This function tries to mimic the C++ way for constructors.
 *
 * To just get a key object, simple do:
 * @code
Key *k = keyNew(0);
// work with it
keyDel (k);
 * @endcode
 *
 * If you want the key object to contain a name, value, comment and other
 * meta info read on.
 *
 * @note When you already have a key with similar properties its
 * easier and cheaper to keyDup() the key.
 *
 * Due to ABI compatibility, the @p Key structure is not defined in kdb.h,
 * only declared. So you can only declare @p pointers to @p Keys in your
 * program, and allocate and free memory for them with keyNew()
 * and keyDel() respectively.
 * See http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html#AEN135
 *
 * You can call it in many different ways depending on the attribute tags you
 * pass as parameters. Tags are represented as the #keyswitch_t values,
 * and tell keyNew() which Key attribute comes next.
 *
 * The simplest and minimum way to use it is with no tags, only a key name:
 * @code
Key *nullKey,*emptyNamedKey;

// Create a key that has no name, is completely empty, but is initialized
nullKey=keyNew(0);
keyDel (nullKey);

// Is the same as above
nullKey=keyNew("", KEY_END);
keyDel (nullKey);

// Create and initialize a key with a name and nothing else
emptyNamedKey=keyNew("user/some/example",KEY_END);
keyDel (emptyNamedKey);
 * @endcode
 *
 * keyNew() allocates memory for a key object and cleans everything up. After
 * that, it processes the given argument list.
 *
 * The Key attribute tags are the following:
 * - keyswitch_t::KEY_TYPE \n
 *   Next parameter is a type of the value. Default assumed is KEY_TYPE_UNDEFINED.
 *   Set this attribute so that a subsequent KEY_VALUE can toggle to keySetString()
 *   or keySetBinary() regarding to keyIsString() or keyIsBinary().
 *   If you don't use KEY_TYPE but a KEY_VALUE follows afterwards, KEY_TYPE_STRING
 *   will be used.
 * - keyswitch_t::KEY_SIZE \n
 *   Define a maximum length of the value. This is especially useful for setting
 *   a binary key. So make sure you use that before you KEY_VALUE for
 *   binary keys.
 * - keyswitch_t::KEY_VALUE \n
 *   Next parameter is a pointer to the value that will be set to the key
 *   If no keyswitch_t::KEY_TYPE was used before,
 *   keyswitch_t::KEY_TYPE_STRING is assumed. If KEY_TYPE was previously
 *   passed with a KEY_TYPE_BINARY,
 *   you should have passed KEY_SIZE before! Otherwise it will be cut of
 *   with first \\0 in string!
 * - keyswitch_t::KEY_UID, @p keyswitch_t::KEY_GID \n
 *   Next parameter is taken as the UID (uid_t) or GID (gid_t) that will
 *   be defined on the key. See keySetUID() and keySetGID().
 * - keyswitch_t::KEY_MODE \n
 *   Next parameter is taken as mode permissions (mode_t) to the key.
 *   See keySetMode().
 * - keyswitch_t::KEY_DIR \n
 *   Define that the key is a directory rather than a ordinary key.
 *   This means its executable bits in its mode are set. This option
 *   allows the key to have subkeys.
 *   See keySetDir().
 * - keyswitch_t::KEY_OWNER \n
 *   Next parameter is the owner. See keySetOwner().
 * - keyswitch_t::KEY_COMMENT \n
 *   Next parameter is a comment. See keySetComment().
 * - keyswitch_t::KEY_REMOVE \n
 *   Mark the key to be removed instead of set it.
 *   See keyRemove().
 * - keyswitch_t::KEY_STAT \n
 *   Mark the key to be stated instead of get it.
 *   See keyStat().
 * - keyswitch_t::KEY_END \n
 *   Must be the last parameter passed to keyNew(). It is always
 *   required, unless the @p keyName is 0.
 *
 * @par Example:
 * @code
KeySet *ks=ksNew(0);

ksAppendKey(ks,keyNew(0));       // an empty key

ksAppendKey(ks,keyNew("user/sw",              // the name of the key
	KEY_END));                      // no more args

ksAppendKey(ks,keyNew("user/tmp/ex1",
	KEY_VALUE,"some data",          // set a string value
	KEY_END));                      // end of args

ksAppendKey(ks,keyNew("user/tmp/ex2",
	KEY_VALUE,"some data",          // with a simple value
	KEY_MODE,0777,                  // permissions
	KEY_END));                      // end of args

ksAppendKey(ks,keyNew("user/tmp/ex4",
	KEY_TYPE,KEY_TYPE_BINARY,	// key type
	KEY_SIZE,7,			// assume binary length 7
	KEY_VALUE,"some data",		// value that will be truncated in 7 bytes
	KEY_COMMENT,"value is truncated",
	KEY_OWNER,"root",		// owner (not uid) is root
	KEY_UID,0,			// root uid
	KEY_END));			// end of args

ksAppendKey(ks,keyNew("user/tmp/ex5",
	KEY_TYPE,
		KEY_TYPE_DIR | KEY_TYPE_BINARY,// dir key with a binary value
	KEY_SIZE,7,
	KEY_VALUE,"some data",		// value that will be truncated in 7 bytes
	KEY_COMMENT,"value is truncated",
	KEY_OWNER,"root",               // owner (not uid) is root
	KEY_UID,0,                      // root uid
	KEY_END));                      // end of args

ksDel(ks);
 * @endcode
 *
 * The reference counter (see keyGetRef()) will be initialized
 * with 0, that means a subsequent call of keyDel() will delete
 * the key. If you append the key to a keyset the reference counter
 * will be incremented by one (see keyInc()) and the key can't be
 * be deleted by a keyDel().
 *
 *@code
Key *k = keyNew(0); // ref counter 0
ksAppendKey(ks, k); // ref counter of key 1
ksDel(ks); // key will be deleted with keyset
 *@endcode
 *
 * If you increment only by one with keyInc() the same as said above
 * is valid:
 *
 *@code
Key *k = keyNew(0); // ref counter 0
keyIncRef(k); // ref counter of key 1
keyDel(k);    // has no effect
keyDecRef(k); // ref counter back to 0
keyDel(k);    // key is now deleted
 *@endcode
 *
 * If you add the key to more keySets:
 *
 *@code
Key *k = keyNew(0); // ref counter 0
ksAppendKey(ks1, k); // ref counter of key 1
ksAppendKey(ks2, k); // ref counter of key 2
ksDel(ks1); // ref counter of key 1
ksDel(ks2); // k is now deleted
 *@endcode
 *
 * or use keyInc() more than once:
 *
 *@code
Key *k = keyNew(0); // ref counter 0
keyIncRef(k); // ref counter of key 1
keyDel (k);   // has no effect
keyIncRef(k); // ref counter of key 2
keyDel (k);   // has no effect
keyDecRef(k); // ref counter of key 1
keyDel (k);   // has no effect
keyDecRef(k); // ref counter is now 0
keyDel (k); // k is now deleted
 *@endcode
 *
 * they key won't be deleted by a keyDel() as long refcounter is not 0.
 *
 * The key's sync bit will always be set for any call, except:
 * @code
Key *k = keyNew(0);
// keyNeedSync() will be false
 * @endcode
 *
 * @param keyName a valid name to the key, or NULL to get a simple
 * 	initialized, but really empty, object 
 * @see keyDel()
 * @return a pointer to a new allocated and initialized Key object,
 * 	or NULL if an invalid @p keyName was passed (see keySetName()).
 * @ingroup key
 * 
 */
Key *keyNew(const char *keyName, ...) {
	Key * k;
	va_list va;

	if (keyName) va_start(va,keyName);
	k = keyVNew (keyName, va);
	if (keyName) va_end (va);

	return k;
}


Key *keyVNew (const char *keyName, va_list va)
{
	Key *key=0;
	type_t type=0;
	keyswitch_t action=0;
	void * value=0;
	ssize_t valueSize=-1;

	key=(Key *)malloc(sizeof(Key));
	if (!key) return 0;
	keyInit(key);

	if (keyName) {
		keySetName(key,keyName);

		action=va_arg(va, keyswitch_t);
		while (action) {
			switch (action) {
				case KEY_TYPE:
					type = (type_t) va_arg(va, type_t);
					keySetType(key, type);
					break;
				case KEY_SIZE:
					valueSize=va_arg(va, size_t);
					break;
				case KEY_VALUE:
					value = va_arg(va, void *);
					if (valueSize>=0 && keyIsBinary(key))
					{
						keySetBinary(key,value, valueSize);
					} else if (keyIsBinary(key)) {
						valueSize = (ssize_t) kdbiStrLen (value);
						keySetBinary(key,value, valueSize);
					} else {
						keySetString(key,value);
					}
					break;
				case KEY_UID:
					keySetUID(key,va_arg(va,uid_t));
					break;
				case KEY_GID:
					keySetGID(key,va_arg(va,gid_t));
					break;
				case KEY_MODE:
					keySetMode(key,va_arg(va, mode_t));
					break;
				case KEY_OWNER:
					keySetOwner(key,va_arg(va,char *));
					break;
				case KEY_COMMENT:
					keySetComment(key,va_arg(va,char *));
					break;
				case KEY_REMOVE:
					keyRemove(key);
					break;
				case KEY_STAT:
					keyStat(key);
					break;
				case KEY_DIR:
					keySetDir(key);
					break;
				default:
#if DEBUG
					fprintf (stderr, "Unknown option in keyNew %ld\n", (long int)action);
#endif
					break;
			}
			action=va_arg(va, keyswitch_t);
		}
	}
	return key;
}

/**
 * Return a duplicate of a key.
 *
 * Memory will be allocated as needed for dynamic properties.
 *
 * The new key will not be member of any KeySet and
 * will start with a new reference counter at 0. A
 * subsequent keyDel() will delete the key.
 *
 * @code
int f (const Key * source)
{
	Key * dup = keyDup (source);
	// work with duplicate
	keyDel (dup);
	// everything related to dup is freed
	// and source is unchanged
}
 * @endcode
 *
 * Like for a new key after keyNew() a subsequent ksAppend()
 * makes a KeySet to take care of the lifecycle of the key.
 *
 * @code
int g (const Key * source, KeySet * ks)
{
	Key * dup = keyDup (source);
	// work with duplicate
	ksAppendKey (ks, dup);
	// ksDel(ks) will also free the duplicate
	// source remains unchanged.
}
 * @endcode
 *
 * Duplication of keys should be preferred to keyNew(),
 * because data like owner can be filled with a copy
 * of the key instead of asking the environment.
 * It can also be optimized in the checks, because the keyname
 * is known to be valid.
 *
 * @param source has to be an initializised source Key
 * @return 0 failure or on NULL pointer
 * @return a fully copy of source on success
 * @see ksAppend(), keyDel()
 * @see keyClear(), keyNew()
 * @ingroup key
 */
Key* keyDup(const Key *source)
{
	Key * dest = 0;

	if (!source) return 0;

	dest = (Key *)malloc(sizeof(Key));
	if (!dest) return 0;
	keyInit(dest);

	/* Copy the struct data, including the "next" pointer */
	*dest=*source;

	/* prepare to set dynamic properties */
	dest->key=
	dest->comment=
	dest->owner=
	dest->data=
	dest->meta=0;

	dest->ksReference = 0;

	if (source->key && keySetName(dest,source->key) == -1) goto memerror;
	if (source->comment && keySetComment(dest,source->comment) == -1) goto memerror;
	if (source->owner && keySetOwner(dest,source->owner) == -1) goto memerror;
	if (source->data && keySetRaw(dest,source->data,source->dataSize) == -1) goto memerror;
	if (source->meta)
	{
		dest->meta = ksDup (source->meta);
	}
	
	return dest;
memerror:
	keyDel (dest);
	return 0;
}




/**
 * Copy or Clear a key.
 *
 * Most often you may prefer keyDup() which allocates
 * a new key and returns a duplication of another key.
 *
 * But when you need to copy into an existing key, e.g.
 * because it was passed by a pointer in a function
 * you can do so:
 *
 * @code
int h (Key *k)
{
	// receive key c
	keyCopy (k, c);
	// the caller will see the changed key k
}
 * @endcode
 *
 * The reference counter will not change for
 * the destination key. Affiliation to keysets
 * are also not affected.
 *
 * When you pass a NULL-pointer as source the
 * data of dest will be cleaned completely and
 * you get a fresh dest key.
 *
 * @code
int g (Key *k)
{
	keyCopy (k, 0);
	// k is now an empty and fresh key
}
 * @endcode
 *
 * @param dest the key which will be written to
 * @param source the key which should be copied
 *     or NULL to clean the destination key
 * @ingroup key
 * @return -1 on failure when a NULL pointer
 *     was passed for dest or a dynamic property could not
 *     be written.
 * @return 0 when dest was cleaned
 * @return 1 when source was successfully copied
 * @see keyDup() to get a duplication of a key
 */
int keyCopy (Key *dest, const Key *source)
{
	size_t destref = 0;

	if (!dest) return -1;
	destref = dest->ksReference;

	/* free everything in dest */
	keySetName (dest, 0);
	keySetComment (dest,0);
	keySetOwner (dest, 0);
	keySetRaw (dest, 0, 0);

	keyClear (dest);

	if (!source) return 0;

	/* copy all data of structure */
	*dest=*source;

	/* Set reference properties */
	dest->ksReference = destref;

	/* prepare to set dynamic properties */
	dest->key=
	dest->comment=
	dest->owner=
	dest->data=0;

	if (keySetName(dest,source->key) == -1) return -1;
	if (keySetComment(dest,source->comment) == -1) return -1;
	if (keySetOwner(dest,source->owner) == -1) return -1;
	if (keySetRaw(dest,source->data,source->dataSize) == -1) return -1;

	return 1;
}





/**
 * A destructor for Key objects.
 *
 * Every key created by keyNew() must be
 * deleted with keyDel().
 *
 * It is save to delete keys which are
 * in a keyset, the number of references
 * will be returned then.
 *
 * It is save to delete a nullpointer,
 * -1 will be returned then.
 *
 * @param key the key object to delete
 * @see keyNew(), keyInc(), keyGetRef()
 * @return the value of the reference counter
 *         if the key is within keyset(s)
 * @return 0 when the key was freed
 * @return -1 on null pointers
 * @ingroup key
 *
 */
int keyDel(Key *key) {
	int rc;

	if (!key) return -1;

	if (key->ksReference > 0)
	{
		return key->ksReference;
	}

	rc=keyClear(key);
	kdbiFree (key);

	return rc;
}

/*
 * Key Object Cleaner.
 *
 * Will reset all internal data.
 *
 * After this call you will receive a fresh
 * key.
 *
 * @code
int f (Key *k)
{
	keyClear (k);
	// you have a fresh key k here
	keySetString (k, "value");
	// the caller will get an empty key k with an value
}
 * @endcode
 *
 * @param key the key object to work with
 * @ingroup key
 */
int keyClear(Key *key)
{
	if (key->key) free(key->key);
	if (key->data) free(key->data);
	if (key->comment) free(key->comment);
	if (key->owner) free(key->owner);
	if (key->meta) ksDel(key->meta);

	memset(key,0,sizeof(Key));
	keyInit (key);

	return 0;
}



/**
 * Increment the viability of a key object.
 *
 * This function is intended for applications
 * using their own reference counter for
 * key objects. With it you can increment
 * the reference and thus avoid destruction
 * of the object in a subsequent keyDel().
 *
 * @code
Key *k;
keyInc (k);
function_that_keyDec(k);
// work with k
keyDel (k); // now really free it
 * @endcode
 *
 * The reference counter can't be incremented
 * once it reached SSIZE_MAX. In that situation
 * nothing will happen and SSIZE_MAX will be
 * returned.
 *
 * @return the value of the new reference counter
 * @return -1 on null pointer
 * @return SSIZE_MAX when maximum exceeded
 * @param key the key object to work with
 * @see keyGetRef(), keyDecRef(), keyDel()
 * @ingroup key
 */
ssize_t keyIncRef(Key *key)
{
	if (!key) return -1;

	if (key->ksReference < SSIZE_MAX) return ++ key->ksReference;
	else return SSIZE_MAX;
}



/**
 * Decrement the viability of a key object.
 *
 * The reference counter can't be decremented
 * once it reached 0. In that situation
 * nothing will happen and 0 will be
 * returned.
 *
 * @return the value of the new reference counter
 * @return -1 on null pointer
 * @return 0 when the key is ready to be freed
 * @param key the key object to work with
 * @see keyGetRef(), keyDel(), keyIncRef()
 * @ingroup key
 */
ssize_t keyDecRef(Key *key)
{
	if (!key) return -1;

	if (key->ksReference>0) return -- key->ksReference;
	else return 0;
}




/**
 * Return how many references the key has.
 *
 * The references will be incremented when ksAppendKey()
 * or ksAppend() uses the key and will be decremented when
 * ksPop() is used.
 *
 * keyDup() will reset the references for dupped
 * key.
 *
 * For your own applications you can use
 * keyIncRef() and keyDelRef() for reference
 * counting. Keys with zero references
 * will be deleted when using keyDel().
 *
 * @param key the key object to work with
 * @return the number of references
 * @return -1 on null pointer
 * @see keyIncRef() and keyDecRef()
 * @ingroup key
 **/
ssize_t keyGetRef(const Key *key)
{
	if (!key) return -1;

	return key->ksReference;
}

