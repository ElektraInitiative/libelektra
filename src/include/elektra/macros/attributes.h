/**
 * @file
 *
 * @brief Elektra macros for compiler attributes (potentially) used in public headers.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_ATTRIBUTES_H
#define ELEKTRA_MACROS_ATTRIBUTES_H

#ifdef __GNUC__
#undef ELEKTRA_SENTINEL
#define ELEKTRA_SENTINEL __attribute__ ((sentinel))
#elif
#define ELEKTRA_SENTINEL
#endif

#ifdef __GNUC__
/** Declares an API as deprecated. */
#define ELEKTRA_DEPRECATED __attribute__ ((deprecated))
#else
#define ELEKTRA_DEPRECATED
#endif

#ifdef __GNUC__
/** Marks a function as never returning, like e.g. exit() */
#define ELEKTRA_ATTRIBUTE_NO_RETURN __attribute__ ((noreturn))
#else
#define ELEKTRA_ATTRIBUTE_NO_RETURN
#endif

#if defined(__GNUC__) || defined(__clang__)
#undef ELEKTRA_WEAK
#define ELEKTRA_WEAK __attribute__ ((weak))
#endif


#endif // ELEKTRA_MACROS_ATTRIBUTES_H
