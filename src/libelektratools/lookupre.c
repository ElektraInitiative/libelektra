/*
 * Lookup for a key which any of its @p where components matches the
 * @p regex regular expression.
 *
 * TODO: Does not work (no example, no testcase)
 * 
 * @deprecated Does not work
 * @param ks the KeySet to lookup into
 * @param where any of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 *        @p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT ORed.
 * @param regexp a regcomp(3) pre-compiled regular expression
 * @param options some @p KDB_O_* ORed options to change lookup behavior.
 *        Currently supported options:
 *        - @p KDB_O_NOSPANPARENT @n
 *          Lookup only keys under ksCurrent()'s parent. If we are in the
 *          begining of the KeySet (ksCurrent()==NULL), this option is
 *          ignored. @p ks must be ksSort()ed (or kdbGetByName() with no
 *          @link KDBOption::KDB_O_NOSORT KDB_O_NOSORT @endlink) for this to work.
 *        - @p KDB_O_ALL @n
 *          Do not be satisfied if reached the end of keyset without finding a
 *          a key that matches, and start over from the begining until ksCurrent()
 * 
 * @return some of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 *         @p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT switches ORed to
 *         indicate @p where the @p regex matched.
 * 
 * @see ksLookupByName(), ksLookupByString(), keyCompare() for other types of
 * 	lookups.
 * @see kdbGetByName(), ksSort()
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
regfree(&regex);

regcomp(&regex,
	"Device/.* /Options/ *",      // only interested in option keys
	REG_ICASE | REG_NOSUB);      // ignore case
regfree(&regex);

regcomp(&regex,
	"^system/folder/.* /basename$", // match real system/ keys that end with 'basename'
	REG_NOSUB);       // always use REG_NOSUB to increase performance
regfree(&regex);

regcomp(&regex,
	"^system/sw/xorg/.* /Screen[0-9]* /Displays/[0-9]* /Depth$", // we want all X.org's depths of all displays of all screens
	REG_ICASE | REG_NOSUB);   // we don't care about the case
regfree(&regex);        // don't forget to free resources

 * @endcode
 */
#if 0
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
		options &= options & ~KDB_O_ALL;
	}
	
	if (options & KDB_O_NOSPANPARENT) {
		/* User wants siblings. Prepare context. */
		parentNameSize=keyGetParentNameSize(init);
		parentName=(char *)malloc(parentNameSize);
		keyGetParentName(init,parentName,parentNameSize);
	}
	
	while ( (walker=ksNext(ks)) != end || (options & KDB_O_ALL) ) {
		if ( walker == NULL ) {
			/* Bottom of list reached
			 * retry lookup from start to cursor */
			ksRewind(ks);
			end=init;
			options &= options & ~KDB_O_ALL;
			continue;
		}
		
		walkerNameSize=keyGetNameSize(walker);
		
		if (options & KDB_O_NOSPANPARENT) {
			/* User wants siblings. Check if walker is a sibling of init. */
			if (walkerNameSize < parentNameSize-1)
				/* we're out of our scope, so abort */
				break;
			
			if (memcmp(parentName,walker->key,parentNameSize-1))
				/* walker has a different parent, so abort */
				break;
		}
	
		if ((where & KEY_SWITCH_NAME) && walker->key)
			if (!regexec(regexp,walker->key,1,&offsets,0))
				match |= KEY_SWITCH_NAME;
		
		/*TODO: Find that bug
		if ((where & KEY_SWITCH_VALUE) && walker->data &&
			!(KEY_TYPE_BINARY <= walker->type && walker->type < KEY_TYPE_STRING))
			if (!regexec(regexp,(char *)walker->data,1,&offsets,0))
				match |= KEY_SWITCH_VALUE;
		*/
		
		if ((where & KEY_SWITCH_OWNER) && keyIsUser(walker))
			if (!regexec(regexp,walker->owner,1,&offsets,0))
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
#endif
