/**
 * @file
 *
 * @brief Elektra macros for symbol versioning.
 *        This file contains the macros needed in public headers.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_SYMVER_H
#define ELEKTRA_MACROS_SYMVER_H

/**
 * Helper macro to create a versioned name of a symbol.
 *
 * @param sym  unversioned name of the symbol
 * @param impl version suffix
 */
#define ELEKTRA_SYMVER(sym, impl) sym##_##impl

#endif // ELEKTRA_MACROS_SYMVER_H
