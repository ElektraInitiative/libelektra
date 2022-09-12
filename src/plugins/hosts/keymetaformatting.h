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

#include <kdb.h>

void elektraAddLineComment (ElektraKeyset * comments, size_t spaces, const char * commentStart, const char * comment);
void elektraAddInlineComment (ElektraKeyset * comments, size_t spaces, const char * commentStart, const char * comment);
size_t elektraCountStartSpaces (const char * line);
ssize_t keySetStringF (ElektraKey * key, const char * format, ...);

#endif /* KEYMETAFORMATTING_H_ */
