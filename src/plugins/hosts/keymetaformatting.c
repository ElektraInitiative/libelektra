/**
 * @file
 *
 * @brief Utility functions for comment metakeys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "keymetaformatting.h"

#include <ctype.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbinternal.h>
#include <string.h>

/**
 * @brief Set a formatted string
 *
 * @param key the key to set the string value
 * @param format NULL-terminated text format string
 * @param ... more arguments
 *
 * @return the size of the string as set (with including 0)
 */
ssize_t keySetStringF (ElektraKey * key, const char * format, ...)
{
	va_list arg_list;

	elektraKeySetMeta (key, "binary", 0);

	va_start (arg_list, format);
	char * p = elektraVFormat (format, arg_list);
	va_end (arg_list);

	if (!p)
	{
		return -1;
	}

	if (key->data.c && !test_bit (key->flags, ELEKTRA_KEY_FLAG_MMAP_DATA))
	{
		elektraFree (key->data.c);
	}

	key->data.c = p;
	key->dataSize = elektraStrLen (key->data.c);
	set_bit (key->flags, ELEKTRA_KEY_FLAG_SYNC);

	return key->dataSize;
}

static void elektraAddCommentInfo (ElektraKeyset * comments, ElektraKey * commentBase, size_t spaces, const char * commentStart, const char * comment)
{
	elektraKeySetString (commentBase, comment);

	if (commentStart)
	{
		/* this comment contains actual comment data */
		ElektraKey * commentStartKey = elektraKeyDup (commentBase, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (commentStartKey, "start");
		elektraKeySetString (commentStartKey, commentStart);
		elektraKeysetAppendKey (comments, commentStartKey);
	}

	elektraKeysetAppendKey (comments, commentBase);

	/* a space comment key is created for each common comment key
	 * and for each line that contains more than one space
	 */
	if (commentStart || spaces > 0)
	{
		ElektraKey * commentSpaceKey = elektraKeyDup (commentBase, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (commentSpaceKey, "space");
		keySetStringF (commentSpaceKey, "%d", spaces);
		elektraKeysetAppendKey (comments, commentSpaceKey);
	}
}


/**
 * Adds a line comment to the supplied comment keyset.
 * The created comment key will always have an array index
 * >= 1. This is because #0 is reserved for inline comments
 *
 * The following rules apply to the comment subkeys (i.e. space, start)
 * - a space key is only omitted for newline comments that do not contain spaces
 *   each other line comment will have a space key (probably with the value 0)
 * - the start key is only present if the comment start sequence is not NULL
 *
 * @param comments the keyset that should hold the created keys
 * @param spaces the number of spaces in the comment
 * @param commentStart the used comment start sequence
 * @param comment the comment data (i.e. the actual comment)
 */
void elektraAddLineComment (ElektraKeyset * comments, size_t spaces, const char * commentStart, const char * comment)
{
	ElektraKey * lineComment;

	/* initialize the comment key */
	if (elektraKeysetGetSize (comments) == 0)
	{
		lineComment = elektraKeyNew ("meta:/comment/#", ELEKTRA_KEY_END);
		elektraArrayIncName (lineComment);
		elektraKeysetAppendKey (comments, lineComment);
		lineComment = elektraArrayGetNextKey (comments);
	}
	else
	{
		// TODO: doing all this every time is very inefficient. Arrayhandling
		// definitely needs to be improved
		ElektraKey * arrayBase = elektraKeyNew ("meta:/comment", ELEKTRA_KEY_END);
		ElektraKeyset * array = elektraArrayGet (arrayBase, comments);
		lineComment = elektraArrayGetNextKey (array);
		elektraKeyDel (arrayBase);
		elektraKeysetDel (array);
	}

	elektraAddCommentInfo (comments, lineComment, spaces, commentStart, comment);
}

/**
 * Adds an inline comment to the supplied comment keyset.
 * The inline comment will always have the index #0. If an
 * inline comment is already present in the supplied comment keyset
 * it will be replaced.
 *
 * @param comments the keyset that should hold the created keys
 * @param spaces the number of spaces in the comment
 * @param commentStart the used comment start sequence
 * @param comment the comment data (i.e. the actual comment)
 */
void elektraAddInlineComment (ElektraKeyset * comments, size_t spaces, const char * commentStart, const char * comment)
{
	ElektraKey * inlineComment = elektraKeyNew ("meta:/comment/#", ELEKTRA_KEY_END);
	elektraArrayIncName (inlineComment);

	elektraAddCommentInfo (comments, inlineComment, spaces, commentStart, comment);
}

/**
 * Counts the number of spaces at the beginning of a string.
 * The function continues until the first non blank character is detected.
 * This means that tabs are counted as normal spaces.
 *
 * @param line the string in which spaces are counted
 * @return the number of spaces before the first non blank character
 */
size_t elektraCountStartSpaces (const char * line)
{
	/* count the number of whitespace characters before the comment */
	size_t spaces = 0;
	size_t lineLen = strlen (line);
	for (size_t i = 0; i < lineLen; i++)
	{
		if (isblank (line[i]))
		{
			spaces++;
		}
		else
		{
			break;
		}
	}
	return spaces;
}
