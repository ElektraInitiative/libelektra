#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <elektra/kdbhelper.h>
#include <kdbassert.h>
#include <kdblogger.h>

/**
 * @internal
 *
 * @brief This function returns a pointer to the first non-whitespace
 *        character in a given string.
 *
 * @pre The parameter `text` must not be `NULL`.
 *
 * @param text The string for which we want to determine the first
 *             non-whitespace character
 *
 * @return A pointer to the first non-whitespace character in `text`
 */
char * elektraLskip (char const * const text)
{
	ELEKTRA_ASSERT (text != NULL, "The Parameter `text` contains `NULL` instead of a valid string.");

	char const * start = text;
	while (isspace ((unsigned char) *start))
	{
		start++;
	}
	return (char *) start;
}

/**
 * @internal
 *
 * @brief This function removes trailing whitespace from the given string.
 *
 * The address stored in the parameter `end` of this function must either
 * point to the last character of the string (the one before `'\0'`) or `NULL`.
 *
 * - If `end` is `NULL`, then the function determines the last character by
 *   calculating the length of the string.
 *
 * - If `end` is specified manually, then the function will cut of the given
 *   string right before the last consecutive whitespace character before
 *   the character stored in `*end`. After the call to this function
 *   the variable `*end` stores the address of the last character of the
 *   stripped string or `'\0'` if the stripped string is empty.
 *
 * @pre The parameter `start` must not be `NULL`.
 * @pre Since this function modifies the string in place, the memory location
 *      containing the given string must be writeable.
 *
 * @param start A pointer to the start of the string from which trailing
 *              whitespace should be removed
 * @param end A pointer to a memory location pointing to the last character
 *              (the one before `'\0'`) of the string this function should
 *              modify or `NULL`
 *
 * @return A pointer to the start of the modified string. This address is
 *         the same as `start`.
 */
char * elektraRstrip (char * const start, char ** end)
{
	ELEKTRA_ASSERT (start != NULL, "The Parameter `start` contains `NULL` instead of a valid string.");

	char * last = (end == NULL || *end == NULL) ? start + strlen (start) - 1 : *end;

	while (start <= last && isspace ((unsigned char) *last))
	{
		last--;
	}
	*(last + 1) = '\0';
	if (end != NULL)
	{
		*end = (last < start) ? start : last;
	}

	return start;
}

/**
 * @internal
 *
 * @brief This function removes leading and trailing whitespace from a given string.
 * *
 * @pre The parameter `text` must not be `NULL`.
 * @pre Since this function modifies the string in place, the memory location
 *      containing the given string must be writeable.
 *
 * @param start A pointer to the start of the string from which trailing
 *              whitespace should be removed
 *
 * @return A pointer to the start of the stripped string.
 */
char * elektraStrip (char * text)
{
	return elektraRstrip (elektraLskip (text), NULL);
}
