/**
 * @file codepoint.h
 *
 * @brief Functions for converting unicode codepoints to utf8.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_CODEPOINT_H
#define ELEKTRA_PLUGIN_TOML_CODEPOINT_H

#include <stdbool.h>

/**
 * @brief Converts the given unicode codepoint string into an utf8 encoded string.
 *
 * @param codepointStr Pointer to the unicode codepoint str.
 * @param codepointLen Length of the given unicode codepoint. Must be 4 or 8.
 * @param utf8 Pointer where the utf8 sequence should be written to.
 *
 * @retval Length of the written utf8 sequence or 0 on error.
 */
int utf8FromUnicode (const char * codepoint, int len, unsigned char * utf8);

/**
 * @brief Checks if the given unicode coinpoint would convert to a valid utf8 string.
 *
 * @param codepointStr Unicode codepoint to check for.
 * @param codepointLen Length of the given unicode codepoint, can be 4 or 8.
 *
 * @retval true if would convert to a valid utf8 sequence
 * 		   false otherwise
 */
bool validUtf8FromUnicode (const char * codepointStr, int codepointLen);

/**
 * @brief Determines the length of an utf8 character by it first character
 *
 * @param head First character of a utf8 character
 *
 * @retval Length of the utf8 character
 */
int utf8LenFromHeadChar (unsigned char head);

#endif // ELEKTRA_PLUGIN_TOML_CODEPOINT_H
