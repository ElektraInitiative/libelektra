/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./validation.h"

/*
 * Lookup for a key which any of its @p where components matches the
 * @p regex regular expression.
 *
 * TODO: Does not work (no example, no test case)
 *
 * @deprecated Does not work
 * @deprecated Remove references to deprecated and removed KEY_COMMENT flag, use KEY_META instead
 * @param ks the KeySet to lookup into
 * @param where any of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 *        @p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT ORed.
 * @param regexp a regcomp(3) pre-compiled regular expression
 *
 * @return some of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 *         @p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT switches ORed to
 *         indicate @p where the @p regex matched.
 *
 * @see ksLookupByName(), ksLookupByString() for other types of
 * 	lookups.
 * @see kdbGetByName()
 *
 * @par Example:
 * @code
KeySet *ks = ksNew (5,
		keyNew ("user:/a", KEY_VALUE, "a", KEY_COMMENT, "does not match", KEY_END),
		keyNew ("user:/b", KEY_VALUE, "  a  ", KEY_COMMENT, "does not match", KEY_END),
		keyNew ("user:/c", KEY_VALUE, "\t\t", KEY_COMMENT, "match", KEY_END),
		keyNew ("user:/d", KEY_VALUE, " \t \t ", KEY_COMMENT, "match", KEY_END),
		KS_END);

Key *match = 0;
regex_t regex;

regcomp(&regex,"^[ \t]*$",REG_NOSUB);

// show the key that match this string
match=ksLookupRE(ks,&regex);

output_key (match);

regfree(&regex); // free regex resources
ksDel (ks);
 * @endcode
 *
 * @par More examples of regular expressions (untesteds of regular expressions (untested)):
 * @code
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
	"^system:/folder/.* /basename$", // match real system:/ keys that end with 'basename'
	REG_NOSUB);       // always use REG_NOSUB to increase performance
regfree(&regex);

regcomp(&regex,
	"^system:/sw/xorg/.* /Screen[0-9]* /Displays/[0-9]* /Depth$", // we want all X.org's depths of all displays of all screens
	REG_ICASE | REG_NOSUB);   // we don't care about the case
regfree(&regex);        // don't forget to free resources

 * @endcode
 */
Key * ksLookupRE (KeySet * ks, const regex_t * regexp, elektraCursor startPos)
{
	regmatch_t offsets;

	for (elektraCursor it = startPos; it < ksGetSize (ks); ++it)
	{
		Key * walker = ksAtCursor (ks, it);
		if (!regexec (regexp, keyString (walker), 1, &offsets, 0)) return walker;
	}

	return 0;
}
