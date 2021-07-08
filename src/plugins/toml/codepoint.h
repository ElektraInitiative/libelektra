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
#include <stdint.h>
#include <stdlib.h>

/**
 * @brief Converts the given unicode codepoint string into an utf8 encoded string.
 *
 * @param codepointStr Pointer to the unicode codepoint hex str.
 * @param codepointLen Length of the given unicode codepoint. Must be 4 or 8.
 * @param utf8 Pointer where the utf8 sequence should be written to.
 *
 * @retval Length of the written utf8 sequence or 0 on error.
 */
int utf8FromUnicode (const char * codepointStr, int codepointLen, char * utf8);

/**
 * @brief Determines the length of an utf8 character by it first character
 *
 * @param head First character of a utf8 character
 *
 * @retval Length of the utf8 character
 */
int utf8LenFromHeadChar (uint8_t head);

/**
 * @brief Checks whether the given byte string is valid UTF-8
 *
 * @param string a sequence of bytes
 * @param len    the length of the byte sequence
 *
 * @retval `true` if the byte sequence is valid UTF-8
 */
bool isValidUtf8 (uint8_t * string, size_t len);

#endif // ELEKTRA_PLUGIN_TOML_CODEPOINT_H
