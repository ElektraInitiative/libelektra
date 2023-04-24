/**
 * @file
 *
 * @brief Source for mini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "./mini.h"

// The definition `_WITH_GETLINE` is required for FreeBSD
#define _WITH_GETLINE
#include <stdio.h>

#include <elektra/ease/old_ease.h>
#include <elektra/kdb/errors.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>
#include <internal/utility/old_utility.h>

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static inline KeySet * elektraMiniContract (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/mini", KEY_VALUE, "mini plugin waits for your orders", KEY_END),
		      keyNew ("system:/elektra/modules/mini/exports", KEY_END),
		      keyNew ("system:/elektra/modules/mini/exports/get", KEY_FUNC, elektraMiniGet, KEY_END),
		      keyNew ("system:/elektra/modules/mini/exports/set", KEY_FUNC, elektraMiniSet, KEY_END),
#include ELEKTRA_README
		      keyNew ("system:/elektra/modules/mini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
		      keyNew ("system:/elektra/modules/mini/config/needs/chars/23", KEY_VALUE, "23", KEY_END), // 23 ↔︎ `#`
		      keyNew ("system:/elektra/modules/mini/config/needs/chars/3B", KEY_VALUE, "3B", KEY_END), // 3B ↔︎ `;`
		      keyNew ("system:/elektra/modules/mini/config/needs/chars/3D", KEY_VALUE, "3D", KEY_END), // 3D ↔︎ `=`
		      keyNew ("system:/elektra/modules/mini/config/needs/chars/5C", KEY_VALUE, "5C", KEY_END), // 5C ↔︎ `\`
		      KS_END);
}

/**
 * @brief This function removes comments marked with `;` or '#' from a
 *        given string by locating the first non-escaped comment character.
 *        It then overwrites this character with `\0`.
 *
 * @pre The parameter `line` must not be `NULL`.
 *
 * @param line The string from which we want to remove line comments.
 *
 * @return A pointer to the first character of the modified version of
 *         line.
 */
static inline char * stripComment (char * line)
{
	ELEKTRA_NOT_NULL (line);

	char * current = line;
	char * before = NULL;

	/* As long as we are not the end of the string and
	   the current character is either not a comment marker or the comment marker was escaped */
	while (*current != '\0' && ((*current != '#' && *current != ';') || (before && *before == '\\')))
	{
		before = current;
		current++;
	}
	*current = '\0';
	return line;
}

/**
 * @brief This function locates the first non-escaped equals
 *        character (`=`) in a given string.
 *
 * @pre The parameter `text` must not be `NULL`.
 *
 * @param text The string in which the equals character should be located
 *
 * @return A pointer to the first unescaped `=` in text or a pointer to
 *         the terminating `\0` of `text` if no such character exists.
 */
static inline char * findUnescapedEquals (char * text)
{
	ELEKTRA_NOT_NULL (text);

	char * equals = text;
	char * before = NULL;

	while (*equals != '\0' && (*equals != '=' || (before && *before == '\\')))
	{
		before = equals++;
	}
	return equals;
}

/**
 * @brief Parse a single line of a text in INI like format (`key = value`) and
 *        store the resulting key value pair in the given key set.
 *
 * The string stored in `line` can also be empty or contain comments denoted
 * by `;` or `#`. The function ignores empty lines. If a line contains non-commented
 * characters that do not follow the pattern `key = value`, then this function will
 * add a warning about this invalid key value pair to `parentKey`.
 *
 * @pre The parameters `line`, `keySet` and `parentKey` must not be `NULL`.
 *
 * @param line A single line string that should be parsed by this function
 * @param lineNumber The lineNumber of the current line of text. This value will
 *                   be used by this function to generate warning messages about
 *                   invalid key value pairs.
 * @param keySet The keyset where the key value pair contained in `line` should
 *               be saved
 * @param parentKey This key is used by this function to store warnings about
 *                  invalid key value pairs
 */
static inline void parseLine (char * line, size_t lineNumber, KeySet * keySet, Key * parentKey)
{
	ELEKTRA_NOT_NULL (line);
	ELEKTRA_NOT_NULL (keySet);
	ELEKTRA_NOT_NULL (parentKey);

	char * pair = elektraStrip (stripComment (line));

	if (*pair == '\0')
	{
		return;
	}

	char * equals = findUnescapedEquals (pair);
	if (*equals == '\0' || equals == pair)
	{
		ELEKTRA_LOG_WARNING ("Ignored line %zu since “%s” does not contain a valid key value pair", lineNumber, pair);
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Line %zu: '%s' is not a valid key value pair", lineNumber, pair);
		return;
	}

	*equals = '\0';

	char * name = elektraRstrip (pair, NULL);
	char * value = elektraLskip (equals + 1);

	Key * key = keyNew (keyName (parentKey), KEY_END);
	keyAddName (key, name);
	keySetString (key, value);
	ELEKTRA_LOG_DEBUG ("Name:  “%s”", keyName (key));
	ELEKTRA_LOG_DEBUG ("Value: “%s”", keyString (key));

	ksAppendKey (keySet, key);
}

/**
 * @brief Parse a file containing text in INI like format (`key = value`).
 *
 * @pre The parameters `file`, `keySet` and `parentKey` must not be `NULL`.
 *
 * @param file The file handle that points to the data this function should parse
 * @param keySet The keyset where the key value pairs saved in `file` should
 *               be saved
 * @param parentKey A key that is used by this function to store warning and error
 *                  information
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the parsing process finished successfully
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to parse `file`
 */
static int parseINI (FILE * file, KeySet * keySet, Key * parentKey)
{
	ELEKTRA_NOT_NULL (file);
	ELEKTRA_NOT_NULL (keySet);
	ELEKTRA_NOT_NULL (parentKey);

	char * line = NULL;
	size_t capacity = 0;
	int errorNumber = errno;

	size_t lineNumber;
	for (lineNumber = 1; getline (&line, &capacity, file) != -1; ++lineNumber)
	{
		ELEKTRA_LOG_DEBUG ("Read Line %zu: %s", lineNumber, line);
		parseLine (line, lineNumber, keySet, parentKey);
	}

	elektraFree (line);

	if (!feof (file))
	{
		ELEKTRA_LOG_WARNING ("%s:%zu: Unable to read line", keyString (parentKey), lineNumber);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unable to read line %zu: %s", lineNumber, strerror (errno));
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Parse a file containing key value pairs in an INI like format and
 *        store the obtained key value pairs in a given key set.
 *
 * @pre The parameters `returned`, and `parentKey` must not be `NULL`.
 *
 * @param returned A key set used to store the key value pairs contained in the
 *                 file specified by the value of `parentKey`
 * @param parentKey The value of this key value pair stores the path to the file
 *                  this function should parse
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the whole parsing process was successful
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if at least one part of the parsing process failed
 */
static int parseFile (KeySet * returned, Key * parentKey)
{
	ELEKTRA_NOT_NULL (returned);
	ELEKTRA_NOT_NULL (parentKey);

	ELEKTRA_LOG ("Read configuration data");
	int errorNumber = errno;
	FILE * source = fopen (keyString (parentKey), "r");

	if (!source || (parseINI (source, returned, parentKey) < 0) | (int) (fclose (source) != 0)) //! OCLint
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Add an error to `parentKey` and restore `errno` if `status` contains
 *        a negative number (write error).
 *
 * @pre The parameter `parentKey` must not be `NULL`.
 *
 * @param status This value will be checked by this function to determine if
 *               the write function (`fprintf`) returned unsuccessfully
 * @param errorNumber A saved value for `errno` this function should restore
 *                    if `status` indicates an write error
 * @param parentKey The key to which this function will add error information
 *                  if `status` indicates an write error
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if status indicates success (`status >= 0`)
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if there was a write error (`status < 0`)
 */
static inline int checkWrite (int status, int errorNumber, Key * parentKey)
{
	ELEKTRA_NOT_NULL (parentKey);

	if (status < 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Store the key value pairs of a key set in a file using an INI like format
 *
 * @pre The parameters `file`, `keySet`, and `parentKey` must not be `NULL`.
 *
 * @param keySet This key set contains the key value pairs that should be stored
 *               in `file`
 * @param parentKey The function uses this key to store error and warning information
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the function was able to store all key value
 *                                       pairs successfully
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to store all key value
 *                                     pairs in `file`
 */
static inline int writeFile (FILE * file, KeySet * keySet, Key * parentKey)
{
	ELEKTRA_NOT_NULL (file);
	ELEKTRA_NOT_NULL (keySet);
	ELEKTRA_NOT_NULL (parentKey);

	int status = 0;
	int errorNumber = errno;

	for (elektraCursor it = 0; it < ksGetSize (keySet) && status >= 0; ++it)
	{
		Key * key = ksAtCursor (keySet, it);
		const char * name = elektraKeyGetRelativeName (key, parentKey);
		ELEKTRA_LOG_DEBUG ("Write mapping “%s=%s”", name, keyString (key));

		status = fprintf (file, "%s=%s\n", name, keyString (key));
	}
	return checkWrite (status, errorNumber, parentKey);
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraMiniGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/mini"))
	{
		ELEKTRA_LOG_DEBUG ("Retrieve plugin contract");
		KeySet * contract = elektraMiniContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return parseFile (returned, parentKey);
}


static bool elektraCheckForInvalidMetaKey (Key * parentKey, KeySet * keySet)
{
	Key * cur = 0;
	for (elektraCursor it = 0; it < ksGetSize (keySet); ++it)
	{
		cur = ksAtCursor (keySet, it);
		const KeySet * metaKeys = keyMeta (cur);
		for (elektraCursor jt = 0; jt < ksGetSize (metaKeys); ++jt)
		{
			// Check if the supported metakey is valid
			// Rename and origname are used by the rename filter plugin
			// Binary is needed for the base64 filter plugin
			// Internal is needed for some checker plugins
			const Key * meta = ksAtCursor (metaKeys, jt);
			const char * pos = (const char *) keyName (meta);
			if (elektraStrNCmp (pos, "meta:/internal/mini", 19) != 0 && elektraStrCmp (pos, "meta:/origname") &&
			    elektraStrNCmp (pos, "meta:/rename", 12) != 0 && elektraStrCmp (pos, "meta:/binary") != 0)
			{
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "The mini storage Plugin doesn't support the meta key %s", pos);
				return false;
			}
		}
	}
	return true;
}

/** @see elektraDocSet */
int elektraMiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraCheckForInvalidMetaKey (parentKey, returned))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ELEKTRA_LOG ("Write configuration data");
	int errorNumber = errno;
	FILE * destination = fopen (keyString (parentKey), "w");

	if (!destination || (writeFile (destination, returned, parentKey) < 0) | (int) (fclose (destination) == EOF)) //! OCLint
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("mini",
		ELEKTRA_PLUGIN_GET,	&elektraMiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraMiniSet,
		ELEKTRA_PLUGIN_END);
}
