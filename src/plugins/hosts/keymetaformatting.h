/**
 * @file
 *
 * @brief Utility functions for comment metakeys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef KEYMETAFORMATTING_H_
#define KEYMETAFORMATTING_H_

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
void elektraAddLineComment (KeySet * comments, size_t spaces, const char * commentStart, const char * comment);
void elektraAddInlineComment (KeySet * comments, size_t spaces, const char * commentStart, const char * comment);
size_t elektraCountStartSpaces (const char * line);
ssize_t keySetStringF (Key * key, const char * format, ...);

#endif /* KEYMETAFORMATTING_H_ */
