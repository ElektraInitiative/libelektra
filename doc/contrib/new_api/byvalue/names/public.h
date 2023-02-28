#ifndef ELEKTRA_NAMES_PUBLIC_H
#define ELEKTRA_NAMES_PUBLIC_H

#include "../core/public.h"

#include <stdlib.h>

/**
 * @brief An `ElektraName` that is still being constructed.
 *
 * @note Because the name is still under construction, this variant of `ElektraName` is passed as pointers.
 */
typedef struct
{
	char * name;
	size_t size;
} ElektraNameBuffer;

static inline ElektraName elektraNameBufferFinish (const ElektraNameBuffer * buffer)
{
	return (ElektraName){ .name = buffer->name, .size = buffer->size };
}

static inline ElektraNameBuffer elektraNameBufferCreate (void)
{
	return (ElektraNameBuffer){ .name = NULL, .size = 0 };
}

void elektraNameBufferAppend (ElektraNameBuffer * buffer, const char * suffix, size_t size);

const char * elektraNameLastPart (ElektraName name, size_t * outSize);

void elektraNameBufferAppendPart (ElektraNameBuffer * buffer, const char * newPart);
void elektraNameBufferRemoveLastPart (ElektraNameBuffer * buffer);
void elektraNameBufferReplaceLastPart (ElektraNameBuffer * buffer, const char * newPart);

/**
 * frees a `ElektraNameBuffer` used by the `elektraNameBuffer*` functions
 * will set `buffer.name = NULL`
 */
void elektraNameBufferFree (ElektraNameBuffer * buffer);

/**
 * `currentPart` must point into `name.name[0..name.size]`
 */
const char * elektraNameNextPart (ElektraName name, const char * currentPart);

/**
 * allocates a new string with the escaped form of @p name in namespace @p ns
 * the returned string must be freed with free()
 */
char * elektraNameEscape (ElektraNamespace ns, ElektraName name);

/**
 * @brief Wrapper for a combination of `ElektraName` and `ElektraNamespace`
 */
typedef struct
{
	ElektraName name;
	ElektraNamespace ns;
} ElektraFullName;

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
ElektraReturnCode elektraNameProcessEscaped (ElektraFullName * dest, const char * escaped, char ** canonical, size_t * canonicalSize,
					     const char ** errorPtr);

/**
 * frees a `ElektraFullName` created by e.g. elektraNameProcessEscaped()
 * will set `name.name.name = NULL`
 */
void elektraFullNameFree (ElektraFullName * name);

#endif // ELEKTRA_NAMES_PUBLIC_H
