/**
 * @file
 *
 * @brief Helper for memory management.
 *
 * Should be always preferred.
 * Can be used for profiling and tracing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDB_HELPER
#define KDB_HELPER

#include <kdbmacros.h>
#include <kdbtypes.h>

#include <stdarg.h>
#include <stddef.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/* Memory handling */
void * elektraMalloc (size_t size);
void * elektraCalloc (size_t size);
void elektraFree (void * ptr);
int elektraRealloc (void ** buffer, size_t size);

char * elektraStrDup (const char * s);
char * elektraStrNDup (const char * s, size_t l);

char * elektraFormat (const char * format, ...) ELEKTRA_ATTRIBUTE_FORMAT (printf, 1, 2);
char * elektraVFormat (const char * format, va_list arg_list);

/* Compare */
int elektraStrCmp (const char * s1, const char * s2);
int elektraStrNCmp (const char * s1, const char * s2, size_t n);
int elektraStrCaseCmp (const char * s1, const char * s2);
int elektraStrNCaseCmp (const char * s1, const char * s2, size_t n);
int elektraMemCaseCmp (const char * s1, const char * s2, size_t size);

/* Len */
size_t elektraStrLen (const char * s);
int elektraWriteArrayNumber (char * newName, kdb_long_long_t newIndex);


#ifdef __cplusplus
}
}
#endif

#endif
