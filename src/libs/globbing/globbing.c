/**
 * @file
 *
 * @brief Library for performing globbing on keynames.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbglobbing.h>
#include <kdbhelper.h>

#include <ctype.h>
#include <fnmatch.h>
#include <stdlib.h>
#include <string.h>

static size_t strcnt (const char * s, char c)
{
	size_t count = 0;
	while ((s = strchr (s + 1, c)) != NULL)
	{
		count++;
	}
	return count;
}

static char * elektraToFnmatchGlob (char * pattern)
{
	char * ptr = pattern;
	while ((ptr = strchr (ptr, '/')) != NULL)
	{
		++ptr;
		if ((*(ptr + 1) == '/' || *(ptr + 1) == '\0') && (*ptr == '#' || *ptr == '_'))
		{
			*ptr = '*'; // replace /#/ and /_/ with /*/
		}
	}
	return pattern;
}

/**
 * @brief checks whether the given string is a valid elektra array item name
 *
 * Valid array items consist of a '#' followed by <code>n</code> underscores ('_'),
 * followed by <code>n+1</code> digits ('0'-'9'). Additionally the digits describe a
 * valid 32-bit Integer (i.e. <code>0 <= x < 2^32</code>).
 *
 * @param name the string to check
 * @retval true if @p name is a valid array item
 * @retval false otherwise
 */
bool isArrayName (const char * name) // TODO: move? definitely useful elsewhere, e.g. reference plugin, libease, etc.
{
	if (name == NULL || *name != '#')
	{
		return false;
	}

	++name;

	int underscores = 0;
	while (*name == '_')
	{
		name++;
		underscores++;
	}

	int digits = 0;
	while (isdigit ((unsigned char) *name))
	{
		name++;
		digits++;
	}

	return digits + underscores + 2 <= ELEKTRA_MAX_ARRAY_SIZE && digits == underscores + 1;
}

/**
 * @brief checks whether a given Key matches a given globbing pattern
 *
 * The globbing patterns for this function are a superset of those from fnmatch(3)
 * used with the FNM_PATHNAME flag:
 * <ul>
 * 	<li> '*' matches any series of characters other than '/'</li>
 * 	<li> '?' matches any single character except '/' </li>
 * 	<li> '#', when used as "/#/" (or "/#" at the end of @p pattern), matches a valid array item </li>
 * 	<li> '_', when used as "/_/"(or "/_" at the end of @p pattern), matches a key part that is <b>not</b> a valid array item </li>
 * 	<li> if the pattern ends with "&#47;**", matching key names may contain arbitrary suffixes </li>
 * </ul>
 *
 * @note '*' cannot match an empty key name part. This also means patterns like "something&#47;*" will
 * not match the key "something". This is because each slash ('/') in the pattern has to correspond to
 * a slash in the canonical key name, which neither end in a slash nor contain multiple slashes in sequence.
 *
 * @param key the Key to match against the globbing pattern
 * @param pattern the globbing pattern used
 * @retval 0 if @p key is not NULL, @p pattern is not NULL and @p pattern matches @p key
 * @retval ELEKTRA_GLOB_NOMATCH otherwise
 *
 * @see isArrayName(), for info on valid array items
 */
int keyGlob (const Key * key, const char * pattern)
{
	if (key == NULL || pattern == NULL)
	{
		return ELEKTRA_GLOB_NOMATCH;
	}

	size_t nameSize = (size_t) keyGetNameSize (key);
	char * name = elektraMalloc (nameSize);
	keyGetName (key, name, nameSize);

	const size_t patternSlashes = strcnt (pattern, '/');

	size_t len = strlen (pattern);
	bool prefixMode = len >= 2 && elektraStrCmp (pattern + len - 3, "/**") == 0;

	char * patternEnd = name;
	for (size_t i = 0; i < patternSlashes; ++i)
	{
		patternEnd = strchr (patternEnd + 1, '/');

		if (patternEnd == NULL)
		{
			// more slashes in pattern, cannot match
			free (name);
			return ELEKTRA_GLOB_NOMATCH;
		}
	}

	if (prefixMode)
	{
		// mark end of relevant part
		*(patternEnd + 1) = '\0';
	}
	else if (strchr (patternEnd + 1, '/') != NULL)
	{
		// more slashes in name, cannot match
		free (name);
		return ELEKTRA_GLOB_NOMATCH;
	}

	char * fnmPattern = elektraToFnmatchGlob (elektraStrDup (pattern));
	int rc = fnmatch (fnmPattern, name, FNM_PATHNAME);
	elektraFree (fnmPattern);

	if (rc == FNM_NOMATCH)
	{
		free (name);
		return ELEKTRA_GLOB_NOMATCH;
	}

	const char * ptr = pattern;
	const char * keyPtr = name;
	while ((ptr = strchr (ptr + 1, '/')) != NULL && (keyPtr = strchr (keyPtr + 1, '/')) != NULL)
	{
		if (*(ptr + 2) == '/' || *(ptr + 2) == '\0')
		{
			if (*(ptr + 1) == '#' && !isArrayName (keyPtr + 1))
			{
				free (name);
				return ELEKTRA_GLOB_NOMATCH;
			}

			if (*(ptr + 1) == '_' && isArrayName (keyPtr + 1))
			{
				free (name);
				return ELEKTRA_GLOB_NOMATCH;
			}
		}
	}

	return 0;
}

/**
 * @brief filters a given KeySet by applying a globbing pattern
 *
 * @param result the KeySet to which the matching keys should be appended
 * @param input the KeySet whose keys should be filtered
 * @param pattern the globbing pattern used
 * @return the number of Keys appended to result or -1,
 * 	   if @p result, @p input or @p pattern are NULL
 *
 * @see keyGlob(), for explanation of globbing pattern
 */
int ksGlob (KeySet * result, KeySet * input, const char * pattern)
{
	if (!result) return ELEKTRA_GLOB_NOMATCH;

	if (!input) return ELEKTRA_GLOB_NOMATCH;

	if (!pattern) return ELEKTRA_GLOB_NOMATCH;

	int ret = 0;
	Key * current;

	cursor_t cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		int rc = keyGlob (current, pattern);
		if (rc == 0)
		{
			++ret;
			ksAppendKey (result, keyDup (current));
		}
	}
	ksSetCursor (input, cursor);
	return ret;
}
