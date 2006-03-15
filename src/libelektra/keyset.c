/***************************************************************************
                          keyset.c  -  Methods for KeySet manipulation
                             -------------------
    begin                : Sun Oct 02 2005
    copyright            : (C) 2005 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/* Subversion stuff

$Id$

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"

/**
 * @defgroup keyset KeySet :: Class Methods
 * @brief Methods to manipulate KeySets.
 * A KeySet is a linked list to group a number of Keys.
 * Key Sets have an @link ksCurrent() internal cursor @endlink to help
 * in the Key navigation.
 *
 * These are the methods to make various manipulations in the objects of class KeySet.
 * Methods for @link ksSort() sorting @endlink, @link ksAppendKeys() merging
 * @endlink, @link ksCompare() comparing @endlink, and @link ksNext() internal
 * cursor manipulation @endlink are provided.
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * @{
 */


/**
 * Allocate, initialize and return a new KeySet object.
 * Objects created with ksNew() must be destroyed with ksDel().
 * 
 * Due to ABI compatibility, the @p KeySet structure is only declared in kdb.h,
 * and not defined. So you can only declare @p pointers to @p KeySets in your
 * program, and allocate and free memory for them with ksNew() and ksDel()
 * respectively.
 * See http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html#AEN135
 *
 * @see ksDel()
 * @return a ready to use KeySet object
 */
KeySet *ksNew() {
	KeySet *new=(KeySet *)malloc(sizeof(KeySet));
	ksInit(new);
	return new;
}



/**
 * A destructor for KeySet objects.
 *
 * Cleans all internal dynamic attributes, keyDel() all contained Keys,
 * and free()s the release the KeySet object memory (that was previously
 * allocated by ksNew()). There is the @p ksFree() macro if you prefer this
 * method name.
 *
 * @see ksNew(), ksClose()
 * @return whatever is returned by ksClose()
 */
int ksDel(KeySet *ks) {
	int rc;
	
	rc=ksClose(ks);
	free(ks);
	
	return rc;
}



/**
 * Return the number of keys that @p ks contains.
 * @see ksNew(), ksDel()
 */
ssize_t ksGetSize(KeySet *ks) {
	return ks->size;
}








/*******************************************
 *           KeySet browsing methods       *
 *******************************************/


/**
 * Resets a KeySet internal cursor.
 * Use it to set the cursor to the begining of the KeySet
 *
 * @return allways 0
 * @see ksNext(), ksCurrent(), kdbMonitorKeys() for an example
 *
 */
int ksRewind(KeySet *ks) {
	ks->cursor=0;
	return 0;
}



/**
 * Returns the next Key in a KeySet.
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksNext() is called the cursor is incremented and the new current Key
 * is returned.
 * You'll get a NULL pointer if the end of KeySet was reached. After that,
 * if ksNext() is called again, it will set the cursor to the begining of
 * the KeySet and the first key is returned.
 *
 * @return the new current Key
 * @see ksRewind(), ksCurrent()
 *
 */
Key *ksNext(KeySet *ks) {
	if (ks->cursor) ks->cursor=ks->cursor->next;
	else ks->cursor=ks->start;

	return ks->cursor;
}



/**
 * Return the current Key
 *
 * @return pointer to the Key pointed by @p ks's cursor
 * @see ksNext(), ksRewind()
 * @see kdbMonitorKeys() for a usage example
 */
Key *ksCurrent(const KeySet *ks) {
	return ks->cursor;
}



/**
 * Return the first key in the KeySet, whithout changing the KeySet's
 * internal cursor.
 * 
 * @see ksTail(), ksCurrent(), ksNext()
 * @see ksRewind() which also resets the internal cursor.
 */
Key *ksHead(KeySet *ks) {
	return ks->start;
}



/**
 * Return the last key in the KeySet, whithout changing the KeySet's
 * internal cursor.
 * 
 * @see ksHead(), ksCurrent(), ksNext()
 */
Key *ksTail(KeySet *ks) {
	return ks->end;
}











/******************************************* 
 *    Looking up Keys inside KeySets       *
 *******************************************/


/**
 * Look for a Key contained in @p ks that matches @p name, starting from
 * @p ks' ksNext() position.
 *
 * The lookup process stops if the end of @p ks is reached. The idea behind
 * it is that your keyset processing logic consider a sorted KeySet and
 * process it in a alphabetical order. So your logic should first lookup
 * key aaa, then bbb, then ccc. This way you avoid going back to a position
 * in an already processed point.
 *
 * This behavior changes if KDB_O_LOOP option is used: if @p ks end is reached
 * start over from the begining until the current key is reached.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor will not move, and a NULL pointer is
 * returned.
 *
 * Cascading is done if the first character is a /. This leads to ignoring
 * the prefix like system/ and user/.
 * @code
        if (kdbGetChildKeys("user/myapp", myConfig, 0 ) == -1)
                BailOut ("Could not get Keys");

        if (kdbGetChildKeys("system/myapp", myConfig, 0 ) == -1)
                BailOut ("Could not get Keys");

        if ((myKey = ksLookupByName (myConfig, "/myapp/key", 0)) == NULL)
                BailOut ("Could not Lookup Key");
 * @endcode
 * This is the way multi user Programs should get there configuration and
 * search after the values. It is guaranteed that more namespaces can be
 * added easily and that all values can be set by admin and user.
 * 
 * The @p ksLookup*() set of methods are designed to let you work with
 * entirely pre-loaded KeySets, so instead of kdbGetKey(), key by key, the
 * idea is to fully kdbGetChildKeys() for your application root key (which is
 * more performatic), and process it all at once with @p ksLookup*().
 *
 * @param ks where to look for
 * @param name key name you are looking for
 * @param options some @p KDB_O_* option bits. Currently suported:
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 *      - @p KDB_O_LOOP @n
 *        Do not be satisfied if reached the end of keyset without finding a
 *        a key that matches, and start over from the begining until ksCurrent()
 * @return pointer to the Key found, 0 otherwise
 * @see ksLookupRE() for powerfull regular expressions based lookups
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByName(KeySet *ks, const char *name, unsigned long options) {
	Key *init=0;
	Key *current=0;
	Key *end=0;
	size_t nameSize;
	size_t currentNameSize;
	char * keyname;

	nameSize=strblen(name);

	init=ks->cursor;
	if ( (init == NULL) || (init == ks->start) ) {
		/* Avoid looping if are already in the begining of the keyset */
		options &= options & ~KDB_O_LOOP;
	}

	while ( ((current=ksNext(ks)) != end) || (options & KDB_O_LOOP) ) {
		if (current == NULL) {
			/* Bottom of list reached
			 * retry lookup from start to cursor */
			ksRewind(ks);
			end=init;
			options &= options & ~KDB_O_LOOP;
			continue;
		}
			
		if (current->key == name) return current; /* for NULLs */
		
		/**This "optimization" makes comparing keys double when equals
		currentNameSize=current->key?strblen(current->key):0;
		if (currentNameSize != nameSize) continue;*/

		if (name [0] == '/') {	/*Cascading search*/
			keyname = strchr(current->key, '/');
		} else {
			keyname = keyStealName (current);
		}

#ifdef VERBOSE
		fprintf (stderr, "Compare %s with %s\n", keyname, name);
#endif

		if ((current->key && name)) {
			if ((options & KDB_O_NOCASE) &&
				!strcasecmp(keyname,name)) return current;
			else if (!strcmp(keyname,name)) return current;
		}
	}

	/* Reached end of KeySet. Put cursor in initial position. */
	ks->cursor=init;

	return 0;
}



/**
 * Lookup for a key which any of its @p where components matches the
 * @p regex regular expression.
 * 
 * @param ks the KeySet to lookup into
 * @param where any of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 * 	@p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT ORed.
 * @param regexp a regcomp(3) pre-compiled regular expression
 * @param options some @p KDB_O_* ORed options to change lookup behavior.
 * 	Currently supported options:
 * - @p KDB_O_NOSPANPARENT @n
 *   Lookup only keys under ksCurrent()'s parent. If we are in the begining of
 *   the KeySet (ksCurrent()==NULL), this option is ignored. @p ks must be
 *   ksSort()ed (or kdbGetChildKeys() with @link KDBOption::KDB_O_SORT
 *   KDB_O_SORT @endlink) for this to work.
 * - @p KDB_O_LOOP @n
 *   Do not be satisfied if reached the end of keyset without finding a
 *   a key that matches, and start over from the begining until ksCurrent()
 * 
 * @return some of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 * 	@p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT switches ORed to
 * 	indicate @p where the @p regex matched.
 * 
 * @see ksLookupByName(), ksLookupByValue(), keyCompare() for other types of
 * 	lookups.
 * @see kdbGetChildKeys(), ksSort()
 * 
 * @par Example:
 * @code
// This code will process all Devices options, device by device.
// Look how we use nested loops, palying with KDB_O_NOSPANPARENT.
// We can do more interesting things when playing with 2 or more regular
// expressions.

regex_t regex;

// you are NOT seeing spaces in this regex
regcomp(&regex,".* /InputDevices/.* /Options/.*",REG_NOSUB);
where=KEY_SWITCH_NAME; // look for it only in key names

ksRewind(ks);
do {
	// show all keys that match this name, and are siblings of the first match
	match=ksLookupRE(ks,where,&regex,0);
	if (match) {
		// We found a device and its first option,
	
		processOption(ksCurrent(ks));
		
		// now process other options of this same device
		do {
			// fetch only the options with same parent with the
			// help of KDB_O_NOSPANPARENT
			match=ksLookupRE(ks,where,&regex,KDB_O_NOSPANPARENT);
			
			if (match) processOption(ksCurrent(ks));
		} while (match);
	}
} while (match);

regfree(&regex); // free regex resources
 * @endcode
 * 
 * @par Examples of regular expressions:
 * @code
regex_t regex;

// The spaces between '*' and '/' and '*' chars are Doxygen mirages :)

regcomp(&regex,
	"some value .* more text",  // match this
	REG_NEWLINE | REG_NOSUB);   // all in a single line

regcomp(&regex,
	"Device/.* /Options/ *",      // only interested in option keys
	REG_ICASE | REG_NOSUB);      // ignore case

regcomp(&regex,
	"^system/folder/.* /basename$", // match real system/ keys that end with 'basename'
	REG_NOSUB);       // allways use REG_NOSUB to increase performance

regcomp(&regex,
	"^system/sw/xorg/.* /Screen[0-9]* /Displays/[0-9]* /Depth$", // we want all X.org's depths of all displays of all screens
	REG_ICASE | REG_NOSUB);   // we don't care about the case


regfree(&regex);        // don't forget to free resources

 * @endcode
 */
uint32_t ksLookupRE(KeySet *ks, uint32_t where,
		const regex_t *regexp, unsigned long options) {
#ifdef HAVE_REGEX_H
	regmatch_t offsets;
	uint32_t match=0;
	Key *init, *walker, *end;
	char *parentName=0;
	size_t walkerNameSize=0,parentNameSize=0;
	
	end=NULL;
	init=ks->cursor;
	if (!init) {
		/* I don't have a parent to match. Ignore this option. */
		options &= options & ~KDB_O_NOSPANPARENT;
	}

	if ( (init == NULL) || (init == ks->start) ) {
		/* Avoid looping if are already in the begining of the keyset */
		options &= options & ~KDB_O_LOOP;
	}
	
	if (options & KDB_O_NOSPANPARENT) {
		/* User wants siblings. Prepare context. */
		parentNameSize=keyGetParentNameSize(init);
		parentName=(char *)malloc(parentNameSize);
		keyGetParentName(init,parentName,parentNameSize);
	}
	
	while ( (walker=ksNext(ks)) != end || (options & KDB_O_LOOP) ) {
		if ( walker == NULL ) {
			/* Bottom of list reached
			 * retry lookup from start to cursor */
			ksRewind(ks);
			end=init;
			options &= options & ~KDB_O_LOOP;
			continue;
		}
		
		walkerNameSize=keyGetNameSize(walker);
		
		if (options & KDB_O_NOSPANPARENT) {
			/* User wants siblings. Check if walker is a sibling of init. */
			if (walkerNameSize < parentNameSize)
				/* we're out of out scope, so abort */
				break;
			
			if (memcmp(parentName,walker->key,parentNameSize))
				/* walker has a different parent, so abort */
				break;
		}
	
		if ((where & KEY_SWITCH_NAME) && walker->key)
			if (!regexec(regexp,walker->key,1,&offsets,0))
				match |= KEY_SWITCH_NAME;
		
		if ((where & KEY_SWITCH_VALUE) && walker->data &&
			!(KEY_TYPE_BINARY <= walker->type && walker->type < KEY_TYPE_STRING))
			if (!regexec(regexp,(char *)walker->data,1,&offsets,0))
				match |= KEY_SWITCH_VALUE;
		
		if ((where & KEY_SWITCH_OWNER) && keyIsUser(walker))
			if (!regexec(regexp,walker->userDomain,1,&offsets,0))
				match |= KEY_SWITCH_OWNER;
		
		if ((where & KEY_SWITCH_COMMENT) && walker->comment)
			if (!regexec(regexp,walker->comment,1,&offsets,0))
				match |= KEY_SWITCH_OWNER;
		
		if (match) return match;
	}
	
	if (parentName) free(parentName);
	ks->cursor=init;
#endif	
	return 0;
}


/**
 * Lookup for a Key contained in @p ks KeySet that matches @p value,
 * starting from ks' ksNext() position.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor won't move, and a NULL pointer is
 * returned.
 * 
 * This method jumps binary keys, unless @p value is NULL.
 * 
 * @par Example:
 * @code
ksRewind(ks);
while (key=ksLookupByValue(ks,"my value",0)) {
	// show all keys which value="my value"
	keyToStream(key,stdout,0);
}
 * @endcode
 * 
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param options some @p KDB_O_* option bits. Currently supported:
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 * @return the Key found, 0 otherwise
 * @see ksLookupByBinaryValue()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByValue(KeySet *ks, const char *value, unsigned long options) {
	Key *init=0;
	Key *current=0;
	size_t size=0;
	
	size=strblen(value);
	init=ks->cursor;
	
	while ((current=ksNext(ks))) {
		if (current->data == value) return current;
		
		if (size != current->dataSize) continue;
		
		if (KEY_TYPE_BINARY<= current->type && current->type < KEY_TYPE_STRING)
			continue;
		
		if ((current->data && value)) {
			if ((options & KDB_O_NOCASE) && 
				!strcasecmp(current->data,value)) return current;
			else if (!strcmp(current->data,value)) return current;
		}
	}
	
	/* reached end of KeySet */
	ks->cursor=init;
	
	return 0;
}



/**
 * Lookup for a Key contained in @p ks KeySet that matches the binary @p value,
 * starting from ks' ksNext() position.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor won't move, and a NULL pointer is
 * returned.
 * 
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param size the size of @p value
 * @param options some @p KDB_O_* option bits, for future use
 * @return the Key found, NULL otherwise
 * @see ksLookupByValue()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByBinaryValue(KeySet *ks, void *value, size_t size,
		unsigned long options) {
	Key *init=0;
	Key *current=0;
	
	init=ks->cursor;
	
	while ((current=ksNext(ks))) {
		if (current->data == value) return current;
		
		if (size != current->dataSize) continue;
		
		if ((current->data && value) && 
				!memcmp(current->data,value,size)) return current;
	}
	
	/* reached end of KeySet, so reset cursor */
	ks->cursor=init;
	
	return 0;
}







/******************************************* 
 *           Filling up KeySets            *
 *******************************************/

 
/**
 * Insert a new Key in the begining of the KeySet. A reference to the key will
 * be stored, and not a copy of the key. So a future ksClose() or ksDel() on
 * @p ks will keyDel() the @p toInsert object.
 * The KeySet internal cursor is not moved.
 *
 * Do not ksInsert() Keys that are already members of other KeySets.
 *
 * @return the size of the KeySet after insertion
 * @param ks KeySet that will receive the key
 * @param toInsert Key that will be inserted into ks
 * @see ksAppend(), ksInsertKeys(), ksAppendKeys(), ksDel(), keyNew()
 *
 */
ssize_t ksInsert(KeySet *ks, Key *toInsert) {
	toInsert->next=ks->start;
	ks->start=toInsert;
	if (!ks->end) ks->end=toInsert;
	return ++ks->size;
}


/**
 * Remove and return the first key of @p ks. If @p ks' cursor was positioned
 * in the poped key, @p ks will be ksRewind()ed.
 * 
 * ksInsert() provides the 'push' bahavior.
 *
 * @return the first key of @p ks, or NULL if @p ks is empty
 * @param ks KeySet to work with
 * @see ksInsert(), ksRewind()
 * @see commandList() for an example
 *
 */
Key *ksPop(KeySet *ks) {
	Key *key=0;
	
	if (ks->start) {
		key=ks->start;
		ks->start=key->next;
		if (ks->end == key) ks->end=0;
		ks->size--;
		if (ks->cursor == key) ks->cursor=0;
	}
	
	return key;
}


/**
 * Remove and return the last key of @p ks. If @p ks' cursor was positioned
 * in the poped key, @p ks will be ksRewind()ed.
 * 
 * ksAppend() provides the 'push' bahavior.
 *
 * @return the last key of @p ks, or NULL if @p ks is empty
 * @param ks KeySet to work with
 * @see ksAppend(), ksRewind()
 * @see commandList() and ksCompare() for examples
 *
 */
Key *ksPopLast(KeySet *ks) {
	Key *key=0;
	Key *prev=0;
	
	/* When ks is empty: */
	if (ks->end == 0) return 0;
	
	/* When ks has only one member: */
	if (ks->start == ks->end) {
		key=ks->end;
		ks->cursor=ks->start=ks->end=0;
		ks->size=0;
		
		return key;
	}
	
	prev=ks->start;
	while (prev->next!=ks->end) prev=prev->next;
	
	key=ks->end;
	prev->next=0;
	ks->end=prev;
	ks->size--;
	if (ks->cursor == key) ks->cursor=0;
	
	return key;
}


/**
 * Transfers all keys from @p toInsert to the begining of @p ks.
 *
 * After this call, @p toInsert will be empty and can be deleted with ksDel().
 *
 * @return the size of the KeySet after insertion
 * @param ks the KeySet that will receive the keys
 * @param toInsert the KeySet that provides the keys that will be transfered
 * @see ksAppend(), ksInsert(), ksAppendKeys()
 *
 */
ssize_t ksInsertKeys(KeySet *ks, KeySet *toInsert) {
	if (toInsert->size) {
		toInsert->end->next=ks->start;
		ks->start=toInsert->start;

		ks->size+=toInsert->size;

		/* Invalidate the old KeySet */
		toInsert->start=toInsert->end=toInsert->cursor=0;
		toInsert->size=0;
	}
	return ks->size;
}



/**
 * Appends a new Key to the end of @p ks. A reference to the key will
 * be stored, and not a private copy. So a future ksClose() or ksDel() on
 * @p ks will keyDel() the @p toAppend object.
 * The KeySet internal cursor is not moved.
 *
 * Do not ksAppend() Keys that are already contained by other KeySets.
 *
 * @return the size of the KeySet after insertion
 * @param ks KeySet that will receive the key
 * @param toAppend Key that will be appended to ks
 * @see ksInsert(), ksInsertKeys(), ksAppendKeys(), keyNew(), ksDel()
 *
 */
ssize_t ksAppend(KeySet *ks, Key *toAppend) {
	toAppend->next=0;
	if (ks->end) ks->end->next=toAppend;
	if (!ks->start) ks->start=toAppend;
	ks->end=toAppend;
	return ++ks->size;
}



/**
 * Transfers all @p toAppend contained keys to the end of the @p ks.
 *
 * After this call, the @p toAppend KeySet will be empty, and can be
 * deleted with ksDel().
 *
 * @return the size of the KeySet after transfer
 * @param ks the KeySet that will receive the keys
 * @param toAppend the KeySet that provides the keys that will be transfered
 * @see ksAppend(), ksInsert(), ksInsertKeys()
 * 
 */
ssize_t ksAppendKeys(KeySet *ks, KeySet *toAppend) {
	if (toAppend->size) {
		if (ks->end) {
			ks->end->next=toAppend->start;
			ks->end=toAppend->end;
		} else {
			ks->end=toAppend->end;
			ks->start=toAppend->start;
		}

		ks->size+=toAppend->size;
		
		/* Invalidate the old KeySet */
		toAppend->start=toAppend->end=toAppend->cursor=0;
		toAppend->size=0;
	}
	return ks->size;
}










/******************************************* 
 *    Other operations                     *
 *******************************************/


/**
 * Compare 2 KeySets.
 *  
 * This method behavior is the following:
 * - A key (by full name) that is present on @p ks1 and @p ks2, and has
 *   something different, will be transfered from @p ks2 to @p ks1, and
 *   @p ks1's (old) version deleted.
 * - Keys present in @p ks1, but not in @p ks2 will be transfered from @p ks1
 *   to @p removed.
 * - Keys that are keyCompare() equal in @p ks1 and @p ks2 will be
 *   keyDel()eted from @p ks2.
 * - Keys present in @p ks2 but not in @p ks1 will
 *   be transfered to @p ks1.
 *
 * In the end, @p ks1 will have all the keys that matter, and @p ks2
 * will be empty.
 *
 * After ksCompare(), you should, in this order:
 * -# ksDel(ks2)
 * -# call kdbSetKeys() on @p ks1 to commit all changed keys
 * -# kdbRemoveKey() for all keys in the @p removed KeySet
 * -# ksDel(removed)
 *
 * @param ks1 first and main KeySet
 * @param ks2 second KeySet
 * @param removed (generally empty) KeySet that will be filled with keys
 * 	removed from @p ks1
 * @see keyCompare()
 * @see commandEdit() at the kdb command
 * @return allways 0
 * @par Example
 * @code
KeySet *ks1,*ks2,*removed;
Key *key;

ks1=ksNew();
ks2=ksNew();
removed=ksNew();

// ...
// Populate ks1 and ks2....
// ...

ksCompare(ks1,ks2,removed);

ksDel(ks2);  // second KeySet is allways empty after ksCompare()
kdbSetKeys(ks1); // commit changed keys
ksDel(ks1);  // don't need ks1 anymore

// Remove all keys that disapeared from ks1...
ksSort(removed); // Sort it first so then we ensure child keys are removed
                 // before their parents
while (key=ksPopLast(removed)) {
	kdbRemoveKey(key);
	keyDel(key);
}

ksDel(removed); // free the KeySet memory
 * @endcode
 */
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed) {
	int flagRemoved=1;
	Key *ks1Cursor=0;
	Key *ks2Cursor=0;

	Key *ks1PrevCursor=0;

	ks1Cursor=ks1->start;
	while (ks1Cursor) {
		Key *ks2PrevCursor=0;
		flagRemoved=1;
		
		for (ks2Cursor=ks2->start; ks2Cursor; ks2Cursor=ks2Cursor->next) {
			uint32_t flags=keyCompare(ks1Cursor,ks2Cursor);
			
			if (!(flags & (KEY_SWITCH_NAME | KEY_SWITCH_DOMAIN))) {
				/* Comparing fullname-equal keys */
				flagRemoved=0; /* key was not removed */
					
				/* First remove from ks2 */
				if (ks2PrevCursor) ks2PrevCursor->next=ks2Cursor->next;
				else ks2->start=ks2Cursor->next;
				if (ks2->end==ks2Cursor) ks2->end=ks2PrevCursor;
				ks2->size--;
					
				/* Now check if we still can find differences between keys, but
				 * we are not interested in the NEEDSYNC flag: he allone is not
				 * enough to determine a key as different */
				if (flags & ~KEY_SWITCH_NEEDSYNC) {
					/* keys are different. Transfer to ks1. */
					
					/* Put in ks1 */
					if (ks1PrevCursor) ks1PrevCursor->next=ks2Cursor;
					else ks1->start=ks2Cursor;
					if (ks1->end==ks1Cursor) ks1->end=ks2Cursor;
					ks2Cursor->next=ks1Cursor->next;
					
					/* delete old version */
					keyDel(ks1Cursor);
					
					/* Reset pointers */
					ks1Cursor=ks2Cursor;
				} else {
					/* Keys are identical. Delete ks2's key. */

					/* Delete ks2Cusrsor */
					keyDel(ks2Cursor);
				}
				/* Don't need to walk through ks2 anymore */
				break;
			}
			ks2PrevCursor=ks2Cursor;
			
		} /* ks2 iteration */
		
		if (flagRemoved) {
			/* This ks1 key was not found in ks2 */
			/* Transfer it from ks1 to removed */
			
			/* Remove from ks1 */
			if (ks1PrevCursor) ks1PrevCursor->next=ks1Cursor->next;
			else ks1->start=ks1Cursor->next;
			if (ks1->end==ks1Cursor) ks1->end=ks1PrevCursor;
			ks1->size--;

			/* Append to removed */
			ksAppend(removed,ks1Cursor);
			
			/* Reset pointers */
			if (ks1PrevCursor) ks1Cursor=ks1PrevCursor->next;
			else ks1Cursor=ks1->start;
		} else {
			ks1PrevCursor=ks1Cursor;
			ks1Cursor=ks1Cursor->next;
		}
	} /* ks1 iteration */
	
	/* Now transfer all remaining ks2 keys to ks1 */
	ksAppendKeys(ks1,ks2);
	
	return 0;
}




/**
 * Writes to @p stream an XML version of the @p ks object.
 *
 * String generated is of the form:
 * @verbatim
<?xml version="1.0" encoding="UTF-8"?>

<!-- Generated by Elektra API. Total of n keys. -->

<keyset xmlns="http://www.libelektra.org"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://www.libelektra.org elektra.xsd">


<key name=...>...</key>
<key name=...>...</key>
<key name=...>...</key>

</keyset>@endverbatim
 *
 * OR, if KDBOptions::KDB_O_HIER is used, the form will be:
 * @verbatim
<?xml version="1.0" encoding="UTF-8"?>

<!-- Generated by Elektra API. Total of n keys. -->

<keyset xmlns="http://www.libelektra.org"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://www.libelektra.org elektra.xsd"

        parent="smallest/parent/name">

<key basename=...>...</key>
<key name=...>...</key> <!-- a key thats not under this keyset's parent -->
<key basename=...>...</key>

</keyset>@endverbatim
 *
 *
 * @param stream where to write output: a file or stdout
 * @param options accepted #KDBOptions ORed:
 * - @p KDBOptions::KDB_O_NUMBERS \n
 *   Do not convert UID and GID into user and group names.
 * - @p KDBOptions::KDB_O_CONDENSED \n
 *   Less human readable, more condensed output.
 * - @p KDBOptions::KDB_O_XMLHEADERS \n
 *   Use it. Include the correct XML headers in the output. If not used, the
 *   <?xml?> and schema info inside the <keyset> object will not be generated.
 * - @p KDBOptions::KDB_O_HIER \n
 *   Will generate a <keyset> node containing a @c parent attribute, and
 *   <key> nodes with a @c basename relative to that @c parent. The @c parent
 *   is calculated by taking the smallest key name in the keyset, so it is a
 *   good idea to have only related keys on the keyset. Otherwise, a valid
 *   consistent XML document still will be generated with regular absolute
 *   @c name attribute for the <key> nodes, due to a
 *   clever keyToStreamBasename() implementation.
 *
 * @see keyToStream()
 * @see commandList() for usage example
 * @return number of bytes written to output, or -1 if some error occurs
 */
ssize_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options) {
	size_t written=0;
	Key *key=0;
	char *codeset;

	/* If nl_langinfo or the CODESET info isn't available we default to UTF-8 
	 * This might not be a very good default, but I'm not sure how else to do it*/
#if defined(HAVE_NL_LANGINFO) && defined(CODESET)
	codeset = nl_langinfo(CODESET);
#else
	codeset = "UTF-8";
#endif
	if (options & KDB_O_XMLHEADERS) {
		written+=fprintf(stream,"<?xml version=\"1.0\" encoding=\"%s\"?>\n",
			codeset);
		written+=fprintf(stream,
			"\n\n<!-- Generated by Elektra API. Total of %d keys. -->\n\n\n\n",ks->size);
		
		written+=fprintf(stream,"<keyset xmlns=\"http://www.libelektra.org\"\n\
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n\
        xsi:schemaLocation=\"http://www.libelektra.org elektra.xsd\"");
		
		/* written+=fprintf(stream,
			"<!DOCTYPE keyset PUBLIC \"-//Avi Alkalay//DTD Elektra 0.1.0//EN\" \"http://elektra.sf.net/dtd/elektra.dtd\">\n\n\n"); */
	} else written+=fprintf(stream,"<keyset");

	if (options & KDB_O_HIER) {
		/*
		Key *smalest=0;
		size_t ssmalest=0;

		for (key=ks->start; key; key=key->next) {
			if (smalest) {
				size_t size=strblen(key->key);
				if (size<ssmalest) {
					smalest=key;
					ssmalest=size;
				}
			} else {
				smalest=key;
				ssmalest=strblen(smalest->key);
			}
		}
	*/
		
		char commonParent[800];

		ksGetCommonParentName(ks,commonParent,sizeof(commonParent));
	
		if (commonParent[0]) {
			written+=fprintf(stream,"\n\n        parent=\"%s\">\n\n\n",
				commonParent);
			for (key=ks->start; key; key=key->next)
				written+=keyToStreamBasename(key,stream,commonParent,0,options);
		} else {
			written+=fprintf(stream,">\n\n\n");
			for (key=ks->start; key; key=key->next)
				written+=keyToStream(key,stream,options);
		}
		
	/*
		if (keyIsUser(smalest) && (options & KDB_O_FULLNAME)) {
			char buffer[800];
			keyGetFullName(smalest,buffer,sizeof(buffer));
			written+=fprintf(stream,"\n\n        parent=\"%s\"",buffer);
		} else
			written+=fprintf(stream,"\n\n        parent=\"%s\"",smalest->key);
	
		
		written+=fprintf(stream,">\n\n\n");
	*/
	} else { /* No KDB_O_HIER*/
		written+=fprintf(stream,">\n\n\n");
		for (key=ks->start; key; key=key->next)
			written+=keyToStream(key,stream,options);
	}
	
	written+=fprintf(stream,"</keyset>\n");
	return written;
}




/**
 * Calculates the common parent to all keys.
 *
 * Given the @p ks KeySet, calculates the parent name for all the keys.
 * So if @p ks contains this keys:
 *
 * @code
 *   system/sw/xorg/Monitors/Monitor1/vrefresh
 *   system/sw/xorg/Monitors/Monitor1/hrefresh
 *   system/sw/xorg/Devices/Device1/driver
 *   system/sw/xorg/Devices/Device1/mode
 * @endcode
 *
 * The common parent is @file system/sw/xorg .
 *
 * On the other hand, if we have this KeySet:
 *
 * @code
 *   system/some/thing
 *   system/other/thing
 *   user/unique/thing
 * @endcode
 *
 * No common parent is possible, so @p returnedCommonParent will contain nothing.
 *
 * This method will work correctly only on @link ksSort() sorted KeySets @endlink.
 *
 * @param returnedCommonParent a pre-allocated buffer that will receive the common parent, if found
 * @param maxSize size of the pre-allocated @p returnedCommonParent buffer
 * @return size in bytes of the parent name, or 0 if there is no common parent,
 * 	or a negative number to indicate an error, then @p errno must be checked.
 *
 */
ssize_t ksGetCommonParentName(const KeySet *ks,char *returnedCommonParent,const size_t maxSize) {
	ssize_t parentSize=0;
	Key *current=0;

	if (keyGetNameSize(ks->start) > maxSize) {
		errno=KDB_RET_TRUNC;
		returnedCommonParent[0]=0;
		return -1;
	}

	strcpy(returnedCommonParent,ks->start->key);
	parentSize=strblen(returnedCommonParent);

	while (*returnedCommonParent) {
		current=ks->start->next;
		while (current) {
			/* Test if a key desn't match */
			if (memcmp(returnedCommonParent,current->key,parentSize-1)) break;
			current=current->next;
		}
		if (current) {
			/* some key failed to be a child */
			/* parent will be the parent of current parent... */
			char *delim=0;

			if ((delim=strrchr(returnedCommonParent,RG_KEY_DELIM))) {
				*delim=0;
				parentSize=strblen(returnedCommonParent);
			} else {
				*returnedCommonParent=0;
				parentSize=0;
			}
		} else {
			/* All keys matched (current==0) */
			/* We have our common parent to return in commonParent */
			return parentSize;
		}
	}
	return parentSize; /* if reached, will be zero */
}





/* Used as a callback by the qsort() function */
int keyCompareByName(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;

	return strcmp(key1->key, key2->key);
}


/**
 * Sorts a KeySet aphabetically by Key name, using qsort().
 *
 * @param ks KeySet to be sorted
 */
void ksSort(KeySet *ks) {
	Key **keys = NULL;
	Key *cursor;
	size_t c=0;

	if (ks->size) {
		keys = (Key **)calloc(ks->size, sizeof(Key *));

		for (cursor=ks->start; cursor; cursor=cursor->next, c++)
			keys[c]=cursor;

		qsort(keys,ks->size,sizeof(Key *),keyCompareByName);

		ks->start=cursor=keys[0];
		for (c=1; c<ks->size; c++) {
			cursor->next=keys[c];
			cursor=cursor->next;
		}
		cursor->next=0;
		ks->end=cursor;
		free(keys);
	}
}






/**
 * KeySet object initializer.
 *
 * You should always use ksNew() instead of ksInit().
 *
 * Every KeySet object that will be used must be initialized first, to setup
 * pointers, counters, etc. After use, all ksInit()ialized KeySets must be
 * cleaned with ksClose().
 * 
 * @see ksNew(), ksClose(), keyInit()
 * @return allways 0
 */
int ksInit(KeySet *ks) {
	ks->start=ks->end=ks->cursor=0;
	ks->size=0;
	
	return 0;
}


/**
 * KeySet object cleaner.
 *
 * Will keyDel() all contained keys, reset internal pointers and counters.
 * 
 * After this call, the @p ks object is ready to be freed by you.
 * 
 * @see keyDel(), ksInit(), keyClose()
 * @see ksAppend() for details on how keys are inserted in KeySets
 * @return allways 0
 */
int ksClose(KeySet *ks) {
	if (ks->size) {
		while (ks->size) {
			Key *destroyer=ks->start;
			ks->start=destroyer->next;
			keyDel(destroyer);
			--ks->size;
		}
	}
	ks->cursor=ks->end=ks->start;
	return 0;
}






/**
 * @} // end of KeySet group
 */
