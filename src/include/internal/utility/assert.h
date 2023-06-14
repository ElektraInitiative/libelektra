/**
 * @file
 *
 * @brief Assertions macros.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBASSERT_H
#define ELEKTRA_KDBASSERT_H

#include <elektra/macros/attributes.h>
#include <elektra/macros/utils.h>

#include <internal/config.h>

#ifdef __cplusplus
extern "C" {
#endif


void elektraAbort (const char * expression, const char * function, const char * file, int line, const char * msg, ...)
	ELEKTRA_ATTRIBUTE_FORMAT (printf, 5, 6) ELEKTRA_ATTRIBUTE_NO_RETURN;

#ifdef __cplusplus
}
#endif

#ifdef ELEKTRA_BMC
#undef NDEBUG
#include <assert.h>
#define ELEKTRA_ASSERT(EXPR, ...) assert (EXPR)
#else
#if DEBUG
#define ELEKTRA_ASSERT(EXPR, ...) ((EXPR)) ? (void) (0) : elektraAbort (ELEKTRA_STRINGIFY (EXPR), __func__, __FILE__, __LINE__, __VA_ARGS__)
#else
#define ELEKTRA_ASSERT(EXPR, ...)
#endif
#define ELEKTRA_NOT_NULL(argument) ELEKTRA_ASSERT (argument, "The variable `" #argument "` contains `NULL`.")
#endif

#endif
