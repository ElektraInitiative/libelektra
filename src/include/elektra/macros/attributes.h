/**
 * @file
 *
 * @brief Elektra macros for compiler attributes.
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

#endif // ELEKTRA_MACROS_ATTRIBUTES_H
