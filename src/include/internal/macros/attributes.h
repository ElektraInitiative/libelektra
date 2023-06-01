/**
 * @file
 *
 * @brief Elektra macros for compiler attributes exclusively used outside of public headers.
 *        Mostly this means attributes which are only used in .c files.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_ATTRIBUTES_INTERNAL_H
#define ELEKTRA_MACROS_ATTRIBUTES_INTERNAL_H

#ifdef __GNUC__
/** Declares a parameter as unused. */
#define ELEKTRA_UNUSED __attribute__ ((unused))
#else
#define ELEKTRA_UNUSED
#endif

#ifdef __GNUC__
/** Declares a switch fallthrough case. */
#define ELEKTRA_FALLTHROUGH __attribute__ ((fallthrough))
#else
#define ELEKTRA_FALLTHROUGH
#endif

#ifdef __GNUC__
#define ELEKTRA_ATTRIBUTE_FORMAT(archetype, stringIndex, firstToCheck) __attribute__ ((format (archetype, stringIndex, firstToCheck)))
#else
#define ELEKTRA_ATTRIBUTE_FORMAT(archetype, stringIndex, firstToCheck)
#endif

#endif // ELEKTRA_MACROS_ATTRIBUTES_INTERNAL_H
