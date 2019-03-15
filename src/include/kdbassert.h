/**
 * @file
 *
 * @brief Assertions macros.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBASSERT_H
#define ELEKTRA_KDBASSERT_H

#include <kdbconfig.h>
#include <kdbmacros.h>

#ifdef __cplusplus
extern "C" {
#endif


void elektraAbort (const char * expression, const char * function, const char * file, const int line, const char * msg, ...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 5, 6))) __attribute__ ((__noreturn__))
#else
#ifdef __clang_analyzer__
	// For scan-build / clang analyzer to detect our assertions abort
	__attribute__ ((analyzer_noreturn))
#endif
#endif
	;

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
