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

#ifndef ELEKTRA_UTILITY_FORMAT_H
#define ELEKTRA_UTILITY_FORMAT_H

#include <elektra/macros/attributes.h>

#include <stdarg.h>
#include <stddef.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

char * elektraFormat (const char * format, ...) ELEKTRA_ATTRIBUTE_FORMAT (printf, 1, 2);
char * elektraVFormat (const char * format, va_list arg_list);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_FORMAT_H
