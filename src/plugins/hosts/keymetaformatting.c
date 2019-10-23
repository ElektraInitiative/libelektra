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

static void elektraAddCommentInfo (KeySet * comments, Key * commentBase, size_t spaces, const char * commentStart, const char * comment)
{
	keySetString (commentBase, comment);

	if (commentStart)
	{
		/* this comment contains actual comment data */
		Key * commentStartKey = keyDup (commentBase);
		keyAddBaseName (commentStartKey, "start");
		keySetString (commentStartKey, commentStart);
		ksAppendKey (comments, commentStartKey);
	}

	ksAppendKey (comments, commentBase);

	/* a space comment key is created for each common comment key
	 * and for each line that contains more than one space
	 */
	if (commentStart || spaces > 0)
	{
		Key * commentSpaceKey = keyDup (commentBase);
		keyAddBaseName (commentSpaceKey, "space");
		keySetStringF (commentSpaceKey, "%d", spaces);
		ksAppendKey (comments, commentSpaceKey);
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
void elektraAddLineComment (KeySet * comments, size_t spaces, const char * commentStart, const char * comment)
{
	Key * lineComment;

	/* initialize the comment key */
	if (ksGetSize (comments) == 0)
	{
		lineComment = keyNew ("comment/#", KEY_META_NAME, KEY_END);
		elektraArrayIncName (lineComment);
		ksAppendKey (comments, lineComment);
		lineComment = elektraArrayGetNextKey (comments);
	}
	else
	{
		// TODO: doing all this every time is very inefficient. Arrayhandling
		// definitely needs to be improved
		Key * arrayBase = keyNew ("comment", KEY_META_NAME, KEY_END);
		KeySet * array = elektraArrayGet (arrayBase, comments);
		lineComment = elektraArrayGetNextKey (array);
		keyDel (arrayBase);
		ksDel (array);
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
void elektraAddInlineComment (KeySet * comments, size_t spaces, const char * commentStart, const char * comment)
{
	Key * inlineComment = keyNew ("comment/#", KEY_META_NAME, KEY_END);
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
