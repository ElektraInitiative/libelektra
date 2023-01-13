#ifndef ELEKTRA_NAMES_PUBLIC_H
#define ELEKTRA_NAMES_PUBLIC_H

#include "../core/public.h"

#include <stdlib.h>

void ElektraNameAppend (ElektraName * name, const char * suffix, size_t size);

// constness of return value follows constness of name
ELEKTRA_CONST char * ElektraNameLastPart (const ElektraName * name, size_t * outSize);
void ElektraNameReplaceLastPart (ElektraName * name, const char * newPart);

// constness of return value follows constness of name
ELEKTRA_CONST char * ElektraNameNextPart (const ElektraName * name, const char * currentPart);

/**
 * allocates a new string with the escaped form of @p name
 */
char * ElektraNameEscape (const ElektraName * name);

/**
 * Verifies that @p escaped is a valid escaped name, and optionally produces
 *   (1) the canonical escaped form
 *   (2) the size of the canonical escaped form
 *   (3) the unescaped form
 *   (4) a pointer to the first error in @p escaped
 *
 * @param dest If != NULL, will contain newly allocated unescaped form on success
 * @param escaped Escaped name to process (NUL-terminated string)
 * @param canonial If != NULL, will be newly allocated string (NUL-terminated) with canonical form of @p escaped
 * @param canonicalSize If != NULL, will be the length of the string written to @p canonical (including NUL-terminator)
 * @param errorPtr If != NULL, will be a pointer into @p escaped, which points to the location of the first error, i.e. the error which is
 * described by the return value
 *
 * @returns 0 if @p escaped is a valid escaped name (other values for different kinds of errors)
 */
ElektraReturnCode ElektraNameProcessEscaped (ElektraName * dest, const char * escaped, char ** canonical, size_t * canonicalSize,
					     const char ** errorPtr);

/**
 * frees a name created by e.g. ElektraNameProcessEscaped()
 */
void ElektraNameFree (ElektraName * name);

#endif // ELEKTRA_NAMES_PUBLIC_H
