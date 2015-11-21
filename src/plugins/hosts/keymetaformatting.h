/**
 * \file
 *
 * @brief Utility functions for comment meta keys
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef KEYMETAFORMATTING_H_
#define KEYMETAFORMATTING_H_

#include <kdb.h>

void elektraAddLineComment(KeySet *comments, size_t spaces, const char *commentStart, const char *comment);
void elektraAddInlineComment(KeySet *comments, size_t spaces, const char *commentStart, const char *comment);
size_t elektraCountStartSpaces(const char* line);

#endif /* KEYMETAFORMATTING_H_ */
