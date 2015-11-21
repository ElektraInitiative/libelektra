/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDB_HELPER
#define KDB_HELPER

#include <stdarg.h>

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

/* Memory handling, please only use these function in plugins */
void *elektraMalloc (size_t size);
void *elektraCalloc (size_t size);
void  elektraFree (void *ptr);
char *elektraStrDup (const char *s);
int elektraRealloc(void **buffer, size_t size);

int elektraStrCmp (const char *s1, const char *s2);
int elektraStrCaseCmp (const char *s1, const char *s2);
int elektraMemCaseCmp (const char *s1, const char *s2, size_t size);
size_t elektraStrLen(const char *s);

char *elektraFormat(const char *format, ...);
char *elektraVFormat(const char *format, va_list arg_list);

#ifdef __cplusplus
}
}
#endif

#endif
