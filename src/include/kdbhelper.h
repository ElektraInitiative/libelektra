/**
 * @file
 *
 * @brief Helper for memory management.
 *
 * Should be always preferred.
 * Can be used for profiling and tracing.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef KDB_HELPER
#define KDB_HELPER

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

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
char * elektraStrDup (const char * s);
int elektraRealloc (void ** buffer, size_t size);
char * elektraFormat (const char * format, ...);
char * elektraVFormat (const char * format, va_list arg_list);

/* Compare */
int elektraStrCmp (const char * s1, const char * s2);
int elektraStrCaseCmp (const char * s1, const char * s2);
int elektraMemCaseCmp (const char * s1, const char * s2, size_t size);

/* Len */
size_t elektraStrLen (const char * s);
int elektraWriteArrayNumber (char * newName, kdb_long_long_t newIndex);


#ifdef __cplusplus
}
}
#endif

#endif
