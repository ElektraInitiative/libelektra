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

#include <elektra/type/types.h>
#include <internal/macros/utils.h>

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
void * elektraMemDup (const void * s, size_t l);

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


/**
 * @brief More lookup options
 *
 * @ingroup proposal
 */
enum elektraLookupOptions
{
	KDB_O_SPEC = 1 << 15,	     ///< Use the passed key as specification, instead of looking up the specification first
	KDB_O_CREATE = 1 << 16,	     ///< Create the key if it was not found
	KDB_O_NOCASCADING = 1 << 17, ///< Disable cascading search for keys starting with /
	KDB_O_NOSPEC = 1 << 18,	     ///< Do not use specification for cascading keys (internal)
	KDB_O_NODEFAULT = 1 << 19,   ///< Do not honor the default spec (internal)
	KDB_O_CALLBACK = 1 << 20,    ///< For spec:/ lookups that traverse deeper into hierarchy (callback in ksLookup())
	KDB_O_OPMPHM = 1 << 21,	  ///< Overrule ksLookup search predictor to use OPMPHM, make sure to set ENABLE_OPTIMIZATIONS=ON at cmake
	KDB_O_BINSEARCH = 1 << 22 ///< Overrule ksLookup search predictor to use Binary search for lookup
};


#ifdef __cplusplus
}
}
#endif

#endif
