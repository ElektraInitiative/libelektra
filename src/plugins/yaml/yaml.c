/**
 * @file
 *
 * @brief Source for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "yaml.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

#ifdef HAVE_LOGGER
#include <libgen.h>
#include <sys/param.h>
#endif

/* -- Data Structures ------------------------------------------------------------------------------------------------------------------- */

/** This enum specifies the possible states of the recursive descent parser. */
typedef enum {
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
	/** Last read text consumed by parser */
	char * text;

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
	ELEKTRA_LOG_DEBUG ("%s:%lu:%lu: " message, strrchr (keyString (data->parentKey), '/') + 1, data->line, data->column, __VA_ARGS__);
#define SET_ERROR_PARSE(data, message, ...)                                                                                                \
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_PARSE, data->parentKey, "%s:%lu:%lu: " message, keyString (data->parentKey), data->line,         \
			    data->column, __VA_ARGS__);

#define PARSE(parser, data)                                                                                                                \
	if (parser->status != OK)                                                                                                          \
	{                                                                                                                                  \
		return data;                                                                                                               \
	}

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

/**
 * @brief Open a file for reading
 *
 * @pre The parameters `parser` and `parser->parentKey` must not be `NULL`
 *
 * @param parser Saves the filename of the file this function opens
 *
 * @retval The updated parsing structure. If there were any errors opening the file, then this function sets the type of the parsing
 * 	   structure to `ERROR_FILE_OPEN`.
 */
static parserType * openFile (parserType * const parser)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");
	ELEKTRA_ASSERT (parser->parentKey, "The Parameter `parentKey` contains `NULL`.");

	parser->file = fopen (keyString (parser->parentKey), "r");

	if (!parser->file)
	{
		ELEKTRA_LOG_WARNING ("Could not open file “%s” for reading: %s", keyString (parser->parentKey), strerror (errno));
		ELEKTRA_SET_ERROR_GET (parser->parentKey);
		errno = parser->errorNumber;
		parser->status = ERROR_FILE_OPEN;
	}
	return parser;
}

/**
 * @brief Close a file handle opened for reading
 *
 * @pre The parameter `parser` must not be `NULL`
 *
 * @param data Saves the file handle this function closes
 *
 * @retval The updated parsing structure. If there were any errors closing the file, then this function sets the type of the parsing
 * 	   structure to `ERROR_FILE_CLOSE`.
 */
static parserType * closeFileRead (parserType * const parser)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");

	if (parser->file && fclose (parser->file) != 0)
	{
		ELEKTRA_SET_ERROR_GET (parser->parentKey);
		errno = parser->errorNumber;
		parser->status = ERROR_FILE_CLOSE;
	}
	return parser;
}

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static KeySet * contractYaml ()
{
	return ksNew (30, keyNew ("system/elektra/modules/yaml", KEY_VALUE, "yaml plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/get", KEY_FUNC, elektraYamlGet, KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/set", KEY_FUNC, elektraYamlSet, KEY_END),
#include ELEKTRA_README (yaml)
		      keyNew ("system/elektra/modules/yaml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

static parserType * setParseError (parserType * const parser)
{
	SET_ERROR_PARSE (parser, "%s", strerror (errno));
	errno = parser->errorNumber;
	parser->status = ERROR_PARSE;
	return parser;
}

static parserType * assertNumberCharsAvailable (parserType * const parser, size_t numberChars)
{
	ELEKTRA_ASSERT (parser, "The parameter `parser` contains `NULL`.");

	char * line = NULL;
	size_t capacity;
	ssize_t numberCharsRead;

	while (parser->bufferCharsAvailable < numberChars && (numberCharsRead = getline (&line, &capacity, parser->file)) != -1)
	{
		size_t bufferCharsAvailable = parser->bufferCharsAvailable + numberCharsRead;
		char * newBuffer = elektraMalloc (bufferCharsAvailable + 1);

		if (!newBuffer) return setParseError (parser);
		strncpy (newBuffer, parser->buffer, parser->bufferCharsAvailable);
		strncpy (newBuffer + parser->bufferCharsAvailable, line, bufferCharsAvailable + 1);

		elektraFree (parser->bufferBase);
		free (line);

		parser->bufferBase = parser->buffer = newBuffer;
		parser->bufferCharsAvailable = bufferCharsAvailable;
	}

	if (feof (parser->file) && parser->bufferCharsAvailable < numberChars) return setParseError (parser);
	return parser;
}

static parserType * getNextChar (parserType * parser)
{
	ELEKTRA_ASSERT (parser && parser->file, "The Parameter `parser` contains `NULL`.");

	if (assertNumberCharsAvailable (parser, 1)->status != OK) return parser;

	parser->bufferCharsAvailable--;
	parser->text = parser->buffer;
	parser->buffer++;

	return parser;
}

static parserType * putBackChars (parserType * parser, size_t numberChars)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");

	parser->bufferCharsAvailable += numberChars;
	parser->buffer -= numberChars;

	return parser;
}

static bool acceptChars (parserType * const parser, char const * const characters, size_t numberCharacters)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");
	ELEKTRA_ASSERT (parser->file, "The Parameter `parser→file` contains `NULL`.");
	ELEKTRA_ASSERT (characters, "The Parameter `characters` contains `NULL`.");

	if (getNextChar (parser)->status != OK) return parser;

	for (size_t charIndex = 0; charIndex < numberCharacters; charIndex++)
	{
		if (*parser->text == characters[charIndex])
		{
			LOG_PARSE (parser, "Accepted character “%c”", characters[charIndex]);
			parser->column++;
			return true;
		}
	}
	LOG_PARSE (parser, "Put back character “%c”", *parser->text);
	putBackChars (parser, 1);

	return false;
}

static bool acceptChar (parserType * const parser, char const character)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");

	return acceptChars (parser, (char[]){ character }, 1);
}

static parserType * expect (parserType * const parser, char const character)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");

	bool found = acceptChar (parser, character);
	if (parser->status != OK)
	{
		return parser;
	}

	if (!found)
	{
		SET_ERROR_PARSE (parser, "Expected character “%c” but found “%c”", character, getc (parser->file));
		parser->status = ERROR_PARSE;
	}

	return parser;
}

static parserType * whitespace (parserType * const parser)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");
	ELEKTRA_ASSERT (parser->file, "The Parameter `file` contains `NULL`.");

	bool found;
	do
	{
		found = acceptChars (parser, (char[]){ ' ', '\t' }, 2);
		if (parser->status != OK)
		{
			break;
		}
	} while (found);

	return parser;
}

static parserType * readUntilDoubleQuote (parserType * const parser)
{
	ELEKTRA_ASSERT (parser, "The Parameter `parser` contains `NULL`.");
	ELEKTRA_ASSERT (parser->file, "The Parameter `parser→file` contains `NULL`.");

	char * previous = NULL;

	size_t numberOfChars = 0;

	char * text = parser->buffer;

	while (getNextChar (parser)->status == OK && (*parser->text != '"' || (previous && *previous == '\\')))
	{
		numberOfChars++;
		LOG_PARSE (parser, "Read character “%c”", *parser->text);
		previous = parser->text;
	}

	if (parser->status != OK)
	{
		return parser;
	}

	*(parser->text + 1) = '\0';
	parser->text = text;

	return parser;
}

static parserType * key (parserType * const parser)
{
	PARSE (whitespace (parser), parser);
	PARSE (expect (parser, '"'), parser);
	PARSE (readUntilDoubleQuote (parser), parser);

	LOG_PARSE (parser, "Read key value “%s”", parser->text);

	return parser;
}

static parserType * pair (parserType * const parser)
{
	PARSE (whitespace (parser), parser);
	PARSE (expect (parser, '{'), parser);
	PARSE (key (parser), parser);

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
	ELEKTRA_ASSERT (returned, "The Parameter `returned` contains `NULL` instead of a valid key set.");
	ELEKTRA_ASSERT (parentKey, "The Parameter `parentKey` contains `NULL` instead of a valid key.");

	ELEKTRA_LOG ("Read configuration data");

	parserType * parser = &(parserType){ .status = OK,
					     .line = 1,
					     .column = 1,
					     .file = NULL,
					     .text = NULL,
					     .bufferBase = NULL,
					     .buffer = NULL,
					     .bufferCharsAvailable = 0,
					     .parentKey = parentKey,
					     .keySet = returned,
					     .errorNumber = errno };
	if (openFile (parser)->status == OK)
	{
		pair (parser);
	}
	closeFileRead (parser);
	if (parser->bufferBase)
	{
		free (parser->bufferBase);
	}

	return parser->status == OK ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/yaml"))
	{
		ELEKTRA_LOG_DEBUG ("Retrieve plugin contract");
		KeySet * contract = contractYaml ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return parseFile (returned, parentKey);
}

/** @see elektraDocSet */
int elektraYamlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yaml)
{
	return elektraPluginExport ("yaml", ELEKTRA_PLUGIN_GET, &elektraYamlGet, ELEKTRA_PLUGIN_SET, &elektraYamlSet, ELEKTRA_PLUGIN_END);
}
