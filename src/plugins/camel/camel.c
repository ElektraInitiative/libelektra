/**
 * @file
 *
 * @brief Source for camel plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "camel.h"

#include <assert.h>
#include <stdbool.h>
// The definition `_WITH_GETLINE` is required for FreeBSD
#define _WITH_GETLINE
#include <stdio.h>
#include <stdlib.h>

#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

#ifdef HAVE_LOGGER
#include <libgen.h>
#include <sys/param.h>
#endif

/* -- Data Structures ------------------------------------------------------------------------------------------------------------------- */

/** This enum specifies the possible states of the recursive descent parser. */
typedef enum
{
	/** Unable to open file */
	ERROR_FILE_OPEN,
	/** Unable to close file */
	ERROR_FILE_CLOSE,
	/** Error while parsing file */
	ERROR_PARSE,
	/** Everything is okay */
	OK
} statusType;

/** This structure saves various data for the recursive descent parser used in this plugin. */
typedef struct
{
	/** The current state of the parsing engine */
	statusType status;

	/** The handle of the opened file */
	FILE * file;
	/** Current line inside `file` */
	size_t line;
	/** Current column inside `line` */
	size_t column;
	/** Start of last text matched by parser */
	char * match;
	/** End of last text matched by parser */
	char * end;
	/** Last key read by parser */
	char * key;
	/** Last value read by parser */
	char * value;

	/** Text buffer for the content saved in `file` */
	char * bufferBase;
	/** Current location in the text buffer */
	char * buffer;
	/** Length of characters still available in the text buffer */
	size_t bufferCharsAvailable;

	/** Saves filename and allows us to emit error information */
	Key * parentKey;
	/** Contains key values pairs we parsed (get direction) or data we need to write back (set direction) */
	KeySet * keySet;

	/** Stores previous value of `errno` */
	int errorNumber;
} parserType;

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define LOG_PARSE(data, message, ...)                                                                                                      \
	ELEKTRA_LOG_DEBUG ("%s:%zu:%zu: " message, strrchr (keyString (data->parentKey), '/') + 1, data->line, data->column, __VA_ARGS__);

#define SET_ERROR_PARSE(data, message, ...)                                                                                                \
	ELEKTRA_SET_ERRORF (PARSING_CODE, data->parentKey, "General parse error: %s:%zu:%zu: " message, keyString (data->parentKey),       \
			    data->line, data->column, __VA_ARGS__);

#define RET_NOK(function)                                                                                                                  \
	if (function->status != OK)                                                                                                        \
	{                                                                                                                                  \
		return parser; /* Requires that the name of the parsing structure is `parser`! */                                          \
	}

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ========
// = Misc =
// ========

/**
 * @brief Set an error specified via the global variable `errno`
 *
 * @pre The parameter `parser` must not be `NULL`.
 *
 * @param parser Saves the parent key this function uses to emit error information
 * @param status Specifies the type of error that this function should set
 *
 * @return An updated version of the variable `parser`
 */
static parserType * setErrorErrno (parserType * const parser, statusType status)
{
	ELEKTRA_NOT_NULL (parser);

	SET_ERROR_PARSE (parser, "%s", strerror (errno));
	errno = parser->errorNumber;
	parser->status = status;
	return parser;
}

/**
 * @brief Set an allocation error
 *
 * @pre The parameter `parser` must not be `NULL`.
 *
 * @param parser Saves the parent key this function uses to emit error information
 * @param status Specifies the size of the last allocation attempt, that caused the error
 *
 * @return An updated version of the variable `parser`
 */
static parserType * setErrorMalloc (parserType * const parser, size_t size)
{
	ELEKTRA_NOT_NULL (parser);

	ELEKTRA_MALLOC_ERROR (parser->parentKey, size);
	parser->status = ERROR_PARSE;
	return parser;
}

// ===========
// = Parsing =
// ===========

/**
 * @brief Extend the text buffer with at least one byte if possible
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the pointer to the buffer location this function tries to fill with at least one character
 *
 * @return An updated version of the variable `parser`
 */
static parserType * bufferChar (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	char * line = NULL;
	size_t capacity;
	ssize_t numberCharsRead;

	while (parser->bufferCharsAvailable < 1 && (numberCharsRead = getline (&line, &capacity, parser->file)) != -1)
	{
		size_t bufferOffset = parser->buffer - parser->bufferBase;
		size_t bufferCharsAvailable = parser->bufferCharsAvailable + numberCharsRead;
		size_t bufferSize = bufferOffset + bufferCharsAvailable + 1;
		if ((parser->bufferBase == 0 && (parser->bufferBase = elektraMalloc (bufferSize)) == NULL) ||
		    ((elektraRealloc ((void **) &parser->bufferBase, bufferSize) < 0)))
		{
			return setErrorMalloc (parser, bufferSize);
		}
		strncpy (parser->bufferBase + bufferOffset, line, numberCharsRead + 1); //! OCLint (constant conditional operator)
		parser->buffer = parser->bufferBase + bufferOffset;
		free (line);

		parser->bufferCharsAvailable = bufferCharsAvailable;
	}

	if (ferror (parser->file)) return setErrorErrno (parser, ERROR_PARSE);
	return parser;
}

/**
 * @brief Read one character from the text buffer if possible
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * getNextChar (parserType * parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	RET_NOK (bufferChar (parser));

	if (parser->bufferCharsAvailable < 1)
	{
		parser->match = NULL;
		return parser;
	}

	if (*parser->buffer == '\n')
	{
		parser->line++;
		parser->column = 1;
	}
	else
	{
		parser->column++;
	}

	parser->bufferCharsAvailable--;
	parser->match = parser->buffer;
	parser->buffer++;

	return parser;
}

/**
 * @brief Put back one character into the buffer
 *
 * @pre The variables `parser` and `parser->buffer` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * putBackChar (parserType * parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->buffer);
	ELEKTRA_ASSERT (parser->buffer - 1 >= parser->bufferBase, "Can not put back more characters than available");

	// We assume that we never put back the newline character
	ELEKTRA_ASSERT (*(parser->buffer - 1) != '\n', "Tried to put back newline character");
	parser->column--;
	parser->bufferCharsAvailable++;
	parser->buffer--;

	return parser;
}

/**
 * @brief Accept one of the characters specified via the string `characters`
 *
 * - If there was an error, then the status of the given parser structure will be updated accordingly.
 * - If one of the characters in `characters` matched the character at the current buffer position, then the function returns a pointer to
 *   the matched character inside the variable `parser->match`. Otherwise `parser->match` will be NULL.
 * - On a match this function increments the current buffer positions.
 *
 * @pre The variables `parser`, `parser->file` and `characters` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 * @param characters Saves a list of characters this function compares with the character at the current buffer position
 *
 * @return An updated version of the variable `parser`
 */
static parserType * acceptChars (parserType * const parser, char const * const characters)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);
	ELEKTRA_NOT_NULL (characters);

	if (getNextChar (parser)->status != OK || !parser->match) return parser;

	char * lastCharacter = parser->match;
	parser->match = NULL;

	if (strchr (characters, *lastCharacter))
	{
		LOG_PARSE (parser, "Accepted character “%c”", *lastCharacter);
		parser->match = lastCharacter;
		return parser;
	}
	LOG_PARSE (parser, "Put back character “%c”", *lastCharacter);
	return putBackChar (parser);
}

/**
 * @brief Assert that the buffer contains one of the characters specified via the string `characters`
 *
 * - If there was an error, then the status of the given parser structure will be updated accordingly.
 * - If none of the characters inside the variable `characters` matched the character at the current buffer position, then this function
 *   will also set an error.
 * - If there was a match, then the function increments the current buffer positions and return the match in the variable `parser->match`.
 *
 * @pre The variables `parser`, `parser->file` and `characters` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 * @param characters Saves a list of characters this function compares with the character at the current buffer position
 *
 * @return An updated version of the variable `parser`
 */
static parserType * expect (parserType * const parser, char const * const characters)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);
	ELEKTRA_NOT_NULL (characters);

	RET_NOK (acceptChars (parser, characters));

	if (!parser->match)
	{
		if (parser->bufferCharsAvailable > 0)
		{
			SET_ERROR_PARSE (parser, "Expected “%s” but found “%c”", characters, *parser->buffer);
		}
		else
		{

			SET_ERROR_PARSE (parser, "Expected “%s” but found end of file instead", characters);
		}
		parser->status = ERROR_PARSE;
	}

	return parser;
}

/**
 * @brief Consume all white space at the current buffer position and increment the buffer positions to account for the read whitespace
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser`, `parser->file` and `characters` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * whitespace (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	while (acceptChars (parser, " \t\n")->status == OK && parser->match)
		; //! OCLINT

	return parser;
}

/**
 * @brief Read a value that starts at the current buffer position and ends with a non-escaped double quote sign
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser`, `parser->buffer` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * content (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);
	ELEKTRA_NOT_NULL (parser->buffer);

	char * previous = parser->buffer;
	size_t numberCharsRead = 0;
	char * text = parser->buffer;

	while (getNextChar (parser)->status == OK && parser->match && (*parser->match != '"' || *previous == '\\'))
	{
		numberCharsRead++;
		LOG_PARSE (parser, "Read character “%c”", *parser->match);
		previous = parser->match;
	}
	RET_NOK (parser);
	if (*(parser->buffer - 1) == '"')
	{
		putBackChar (parser);
		numberCharsRead--;
	}

	parser->end = text + numberCharsRead;
	LOG_PARSE (parser, "End: “%c”", *parser->end);
	parser->match = text;

	return parser;
}

/**
 * @brief Read a value that starts and ends with a non-escaped double quote sign.
 *
 * - If there was an error, then the status of the given parser structure will be updated accordingly.
 * - If the value was read successfully, then the start (position right after the first `"`) and end (position right before the ending
 *   `"`) of the content will be saved in the variables `parser->match` and `parser->end`.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * doubleQuoted (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	RET_NOK (expect (parser, "\""));
	RET_NOK (content (parser));
	char * text = parser->match;
	RET_NOK (expect (parser, "\""));
	parser->match = text;

	return parser;
}

/**
 * @brief Save a copy of the string specified via the variables `parser->match` and `parser->end` in `location`
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 * @param location The location where this function should save the copy of the string
 *
 * @return An updated version of the variable `parser`
 */
static parserType * saveText (parserType * const parser, char ** location)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->match);
	ELEKTRA_NOT_NULL (parser->end);
	ELEKTRA_ASSERT (parser->end - parser->match >= -1, "The string specified via parser->match and parser->end has negative length");
	ELEKTRA_NOT_NULL (location);

	size_t length = parser->end - parser->match + 1;
	if (*location) elektraFree (*location);
	*location = elektraMalloc (length + 1);
	if (!*location) return setErrorMalloc (parser, length + 1);

	strncpy (*location, parser->match, length); //! OCLint (constant conditional operator)
	(*location)[length] = '\0';

	return parser;
}

/**
 * @brief Read a double quoted value that starts and ends with optional whitespace characters
 *
 * - The content of the double quoted value will be stored at the address specified via the variable `location`.
 * - If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * doubleQuotedSpace (parserType * const parser, char ** location)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	RET_NOK (whitespace (parser));
	RET_NOK (doubleQuoted (parser));
	RET_NOK (saveText (parser, location));
	RET_NOK (whitespace (parser));

	return parser;
}

/**
 * @brief Read a key (including leading and trailing whitespace) and save the result in the variable `parser->key`.
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * key (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	return doubleQuotedSpace (parser, &parser->key);
}

/**
 * @brief Read a value (including leading and trailing whitespace) and save the result in the variable `parser->value`.
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * value (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	return doubleQuotedSpace (parser, &parser->value);
}

/**
 * @brief Read a key value pair and save them in the key set `parser->keySet`
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * pair (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	RET_NOK (key (parser));
	LOG_PARSE (parser, "Read key “%s”", parser->key);

	RET_NOK (expect (parser, ":"));
	RET_NOK (value (parser));
	LOG_PARSE (parser, "Read value “%s”", parser->value);

	Key * key = keyNew (keyName (parser->parentKey), KEY_END);
	keyAddName (key, parser->key);
	keySetString (key, parser->value);
	ELEKTRA_LOG_DEBUG ("Name:  “%s”", keyName (key));
	ELEKTRA_LOG_DEBUG ("Value: “%s”", keyString (key));
	ksAppendKey (parser->keySet, key);

	return parser;
}

/**
 * @brief Read optional key value pairs and save them in the key set `parser->keySet`
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * optionalAdditionalPairs (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	while (acceptChars (parser, ",")->status == OK && parser->match)
	{
		RET_NOK (pair (parser));
	}
	return parser;
}

/**
 * @brief Read a list of key value pairs and save them in the key set `parser->keySet`
 *
 * If there was an error, then the status of the given parser structure will be updated accordingly.
 *
 * @pre The variables `parser` and `parser->file` must not be `NULL`
 *
 * @param parser Saves the parsing information this function operates on
 *
 * @return An updated version of the variable `parser`
 */
static parserType * pairs (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->file);

	RET_NOK (whitespace (parser));
	RET_NOK (expect (parser, "{"));

	RET_NOK (pair (parser));

	RET_NOK (optionalAdditionalPairs (parser));

	RET_NOK (expect (parser, "}"));
	LOG_PARSE (parser, "“%s: %s”", parser->key, parser->value);

	return parser;
}

// =====================
// = Resource Handling =
// =====================

/**
 * @brief Open a file for reading
 *
 * @pre The variables `parser` and `parser->parentKey` must not be `NULL`
 *
 * @param parser Saves the filename of the file this function opens
 *
 * @return The updated parsing structure. If there were any errors opening the file, then this function sets the type of the parsing
 * 	   structure to `ERROR_FILE_OPEN`.
 */
static parserType * openFile (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);
	ELEKTRA_NOT_NULL (parser->parentKey);

	parser->file = fopen (keyString (parser->parentKey), "r");

	if (!parser->file) setErrorErrno (parser, ERROR_FILE_OPEN);

	return parser;
}

/**
 * @brief Free allocated resources
 *
 * @pre The parameter `parser` must not be `NULL`
 *
 * @param parser Contains resources this function frees
 *
 * @return The updated parsing structure. If there were any errors closing the file specified via `parser`, then this function sets
 * the type of the parsing structure to `ERROR_FILE_CLOSE`.
 */
static parserType * cleanup (parserType * const parser)
{
	ELEKTRA_NOT_NULL (parser);

	if (parser->file && fclose (parser->file) != 0) setErrorErrno (parser, ERROR_FILE_CLOSE);
	if (parser->bufferBase) elektraFree (parser->bufferBase);
	if (parser->key) elektraFree (parser->key);
	if (parser->value) elektraFree (parser->value);

	return parser;
}

/**
 * @brief Parse a file containing data specified in a very basic subset of YAML and store the obtained data in a given key set.
 *
 * @pre The parameters `returned`, and `parentKey` must not be `NULL`.
 *
 * @param returned A key set used to store the data contained in the file specified by the value of `parentKey`
 * @param parentKey The value of this key value pair stores the path to the file this function should parse
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the whole parsing process was successful
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if at least one part of the parsing process failed
 */
static int parseFile (KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	ELEKTRA_NOT_NULL (returned);
	ELEKTRA_NOT_NULL (parentKey);

	ELEKTRA_LOG ("Read configuration data");

	parserType * parser = &(parserType){ .status = OK,
					     .line = 1,
					     .column = 1,
					     .file = NULL,
					     .match = NULL,
					     .bufferBase = NULL,
					     .buffer = NULL,
					     .bufferCharsAvailable = 0,
					     .parentKey = parentKey,
					     .keySet = returned,
					     .errorNumber = errno };

	if (openFile (parser)->status == OK) pairs (parser);
	cleanup (parser);

	return parser->status == OK ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static KeySet * contractCamel (void)
{
	return ksNew (30, keyNew ("system/elektra/modules/camel", KEY_VALUE, "camel plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/camel/exports", KEY_END),
		      keyNew ("system/elektra/modules/camel/exports/get", KEY_FUNC, elektraCamelGet, KEY_END),
		      keyNew ("system/elektra/modules/camel/exports/set", KEY_FUNC, elektraCamelSet, KEY_END),
#include ELEKTRA_README
		      keyNew ("system/elektra/modules/camel/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

/**
 * @brief Store the key value pairs of a key set in a file using a YAML sequence
 *
 * @pre The parameters `file`, `keySet`, and `parentKey` must not be `NULL`.
 *
 * @param keySet This key set contains the key value pairs that should be stored in `file`
 * @param parentKey The function uses this key to determine the relative name of a key in `keySet`
 *
 * @return The function returns a positive number (including 0) on success or a negative number if there was a problem writing the file.
 */
static int writeFile (FILE * file, KeySet * keySet, Key * parentKey)
{
	ELEKTRA_NOT_NULL (file);
	ELEKTRA_NOT_NULL (keySet);
	ELEKTRA_NOT_NULL (parentKey);

	ksRewind (keySet);

	int status = fprintf (file, "{\n");
	bool first = true;
	for (Key * key; status >= 0 && (key = ksNext (keySet)) != 0;)
	{
		const char * name = elektraKeyGetRelativeName (key, parentKey);
		ELEKTRA_LOG_DEBUG ("Write mapping “\"%s\" : \"%s\"”", name, keyString (key));
		status = fprintf (file, "%s \"%s\" : \"%s\"\n", first ? " " : ",", name, keyString (key));
		first = false;
	}
	return status < 0 ? status : fprintf (file, "}");
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraCamelGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/camel"))
	{
		ELEKTRA_LOG_DEBUG ("Retrieve plugin contract");
		KeySet * contract = contractCamel ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return parseFile (returned, parentKey);
}

/** @see elektraDocSet */
int elektraCamelSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	ELEKTRA_LOG ("Write configuration data");
	int errorNumber = errno;
	FILE * destination = fopen (keyString (parentKey), "w");

	// The bitwise or in the next line is correct, since we need to close the file even if writing fails
	if (!destination || (writeFile (destination, returned, parentKey) < 0) | (fclose (destination) == EOF)) //! OCLint
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("camel", ELEKTRA_PLUGIN_GET, &elektraCamelGet, ELEKTRA_PLUGIN_SET, &elektraCamelSet,
				    ELEKTRA_PLUGIN_END);
}
