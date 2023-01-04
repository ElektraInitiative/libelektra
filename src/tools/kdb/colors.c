/**
 * @file
 *
 * @brief Implementation of ANSI escape helper functions in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <colors.h>
#include <kdberrors.h>
#include <string.h>

const char * formatString (char ** buffer, int colorMode, const char * escapeCode, const char * text)
{
	if (colorMode == CLI_COLOR_NEVER) return text;
	if (*buffer == NULL)
	{
		*buffer = elektraMalloc (4096);
	}
	char * tmp = elektraMalloc (elektraStrLen (escapeCode) + elektraStrLen (text) + elektraStrLen (RESET) + 1);
	strcpy (tmp, escapeCode);
	strcat (tmp, text);
	strcat (tmp, RESET);

	**buffer = '\0';
	strcpy (*buffer, tmp);
	elektraFree (tmp);

	return *buffer;
}
