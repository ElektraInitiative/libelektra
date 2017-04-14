#include <ctype.h>
#include <stdlib.h>
#include <string.h>

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
	while (isspace ((unsigned char)*start))
	{
		start++;
	}
	return (char *)start;
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

	while (start <= last && isspace ((unsigned char)*last))
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

static inline ssize_t occurrences (char const * const text, char const * const pattern, ssize_t patternLength)
{
	ssize_t occurrences = 0;
	char const * current = text;
	while ((current = strstr (current, pattern)) != NULL)
	{
		current += patternLength;
		occurrences++;
	}
	return occurrences;
}

/**
 * @internal
 *
 * @brief This function returns a new string replacing every occurrence of `pattern`
 *        in `text` with `replacement`.
 *
 * @pre The parameter `text` must not be `NULL`.
 * @pre The parameter `pattern` must not be `NULL` or an empty string.
 * @pre The parameter `replacement` must not be `NULL`.
 *
 * @param text The start of a string where each occurrence of `pattern` should be
 *             replaced with `replacement`
 * @param pattern A pattern, that describes which part of `text` should be replaced
 * @param replacement This pattern describes the replacement that should be used for
 *                    `pattern`
 *
 * @return A pointer to the start of a newly allocated string. The new string
 *         is identical to the string `text` except, that every occurrence of
 *         pattern was replaced with `replacement`.
 */
char * elektraReplace (char const * const text, char const * const pattern, char const * const replacement)
{
	ELEKTRA_ASSERT (text != NULL, "The Parameter `text` contains `NULL` instead of a valid string.");
	ELEKTRA_ASSERT (pattern != NULL, "The Parameter `pattern` contains `NULL` instead of a valid string.");
	ELEKTRA_ASSERT (pattern != '\0',
			"The Parameter `pattern` contains an empty string. Please provide a string with a minimum length of 1 character.");
	ELEKTRA_ASSERT (replacement != NULL, "The Parameter `replacement` contains `NULL` instead of a valid string.");

	ssize_t patternLength = strlen (pattern);
	ssize_t numberOccurrences = occurrences (text, pattern, patternLength);
	ELEKTRA_LOG_DEBUG ("Found “%lu” occurrences of “%s” in “%s”", numberOccurrences, pattern, text);

	ssize_t replacementLength = strlen (replacement);
	ssize_t textLength = strlen (text);
	char * result = malloc ((textLength + numberOccurrences * (replacementLength - patternLength) + 1) * sizeof (char));
	if (result == NULL)
	{
		return NULL;
	}

	char const * current = text;
	char const * before = text;
	char * destination = result;
	while ((current = strstr (current, pattern)) != NULL)
	{
		destination = stpncpy (destination, before, current - before);       //! OCLint (False Positive: Constant Conditional OP)
		destination = stpncpy (destination, replacement, replacementLength); //! OCLint (False Positive: Constant Conditional OP)

		current += patternLength;
		before = current;
	}
	strncpy (destination, before, text + textLength - before + 1); //! OCLint (False Positive: Constant Conditional OP)

	return (char *)result;
}
