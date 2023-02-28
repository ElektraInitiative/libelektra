#ifndef ELEKTRA_NAMES_PUBLIC_H
#define ELEKTRA_NAMES_PUBLIC_H

#include "../core/public.h"

#include <stdlib.h>

typedef struct ElektrNameBuffer ElektraNameBuffer;


/**
 * allocates a new ElektraNameBuffer for building a name
 */
ElektraNameBuffer * elektraNameBufferNew (void);

/**
 * creates a new ElektraName with elektraNameNew() using the name in @p buffer
 *
 * if the @p buffer doesn't contain a valid name, returns `NULL`
 */
ElektraName * elektraNameBufferFinish (const ElektraNameBuffer * buffer);

/**
 * frees a `ElektraNameBuffer` used by the `elektraNameBuffer*` functions
 * will set `buffer.name = NULL`
 */
void elektraNameBufferFree (ElektraNameBuffer * buffer);


void elektraNameBufferClear (ElektraNameBuffer * buffer);
void elektraNameBufferAppend (ElektraNameBuffer * buffer, const char * suffix, size_t size);

void elektraNameBufferSetNamespace (ElektraNameBuffer * buffer, ElektraNamespace ns);

static inline void elektraNameBufferSetName (ElektraNameBuffer * buffer, const char * name, size_t size)
{
	elektraNameBufferClear (buffer);
	elektraNameBufferAppend (buffer, name, size);
}

const char * elektraNameLastPart (ElektraName name, size_t * outSize);

void elektraNameBufferAppendPart (ElektraNameBuffer * buffer, const char * newPart);
void elektraNameBufferRemoveLastPart (ElektraNameBuffer * buffer);
void elektraNameBufferReplaceLastPart (ElektraNameBuffer * buffer, const char * newPart);


/**
 * `currentPart` must point into `name.name[0..name.size]`
 */
const char * elektraNameNextPart (const ElektraName * name, const char * currentPart);

/**
 * allocates a new string with the escaped form of @p name in namespace @p ns
 * the returned string must be freed with free()
 */
char * elektraNameEscape (ElektraNamespace ns, const ElektraName * name);

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
ElektraReturnCode elektraNameProcessEscaped (ElektraNameBuffer * dest, const char * escaped, char ** canonical, size_t * canonicalSize,
					     const char ** errorPtr);


#endif // ELEKTRA_NAMES_PUBLIC_H
