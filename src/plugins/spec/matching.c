#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/globbing.h>

#include <string.h>

#ifdef __MINGW32__
bool specMatches (Key * specKey, Key * otherKey)
{
	/**
	 * Known limitation: For MINGW builds fnmatch.h does not exist. Therefore, globbing can't be used.
	 * This means that there is no support for # and _ in key names.
	 * This function was copied from 68e9dff, doesn't use globbing and therefore doesn't require the globbing library which is not
	 * compatible with Windows:
	 */

	if (specKey == NULL || otherKey == NULL)
	{
		return false;
	}

	const char * spec = keyUnescapedName (specKey);
	size_t specNsLen = strlen (spec) + 1;
	spec += specNsLen; // skip namespace
	const char * other = keyUnescapedName (otherKey);
	size_t otherNsLen = strlen (other) + 1;
	other += otherNsLen; // skip namespace
	size_t const specSize = keyGetUnescapedNameSize (specKey) - specNsLen;
	size_t const otherSize = keyGetUnescapedNameSize (otherKey) - otherNsLen;

	return specSize == otherSize && memcmp (spec, other, specSize) == 0;
}
#else
/**
 * Check whether the {@link otherKey} matches the {@link specKey}.
 *
 * @param specKey specification key
 * @param otherKey the other key to match the specification key with
 * @retval 1 - if the keys match
 * @retval 0 - if the keys do not match
 */
bool specMatches (Key * specKey, Key * otherKey)
{
	if (specKey == NULL || otherKey == NULL)
	{
		return false;
	}

	// ignore namespaces for globbing
	char * untilFirstSlash = strchr (keyName (otherKey), '/');
	if (untilFirstSlash == NULL)
	{
		return NULL;
	}
	Key * globKey = keyNew (untilFirstSlash, KEY_END);

	bool matches = elektraKeyGlob (globKey, strchr (keyName (specKey), '/')) == 0;

	keyDel (globKey);

	return matches;
}
#endif
