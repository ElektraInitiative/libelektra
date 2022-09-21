/**
 * @file
 *
 * @brief Implementation of ANSI escape helper functions in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <colors.h>
#include <kdberrors.h>
#include <stdio.h>

const char * formatString (char ** buffer, int colorMode, const char * escapeCode, const char * text)
{
	if (colorMode == CLI_COLOR_NEVER) return text;

	size_t len = elektraStrLen (escapeCode) + elektraStrLen (text) + elektraStrLen (RESET) + 1;
	if (elektraRealloc ((void **) buffer, len) == -1)
	{
		return "error allocating memory";
	}

	snprintf (*buffer, len, "%s%s%s", escapeCode, text, RESET);

	return *buffer;
}
